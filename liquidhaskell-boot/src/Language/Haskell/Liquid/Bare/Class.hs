{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections     #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Haskell.Liquid.Bare.Class
  ( makeClasses
  , makeSpecDictionaries
  , makeDefaultMethods
  , makeMethodTypes
  )
  where

import           Data.Bifunctor
import           Data.Bitraversable
import qualified Data.Maybe                                 as Mb
import qualified Data.List                                  as L
import qualified Data.HashMap.Strict                        as M

import qualified Language.Fixpoint.Types                    as F
import qualified Language.Fixpoint.Types.Visitor            as F

import           Language.Haskell.Liquid.Types.Dictionaries
import qualified Language.Haskell.Liquid.GHC.Misc           as GM
import qualified Liquid.GHC.API            as Ghc
import           Language.Haskell.Liquid.Misc
import           Language.Haskell.Liquid.Types.DataDecl
import           Language.Haskell.Liquid.Types.Errors
import           Language.Haskell.Liquid.Types.Names
import           Language.Haskell.Liquid.Types.RefType
import           Language.Haskell.Liquid.Types.RType
import           Language.Haskell.Liquid.Types.RTypeOp
import           Language.Haskell.Liquid.Types.Types
import           Language.Haskell.Liquid.Types.Visitors

import qualified Language.Haskell.Liquid.Measure            as Ms
import           Language.Haskell.Liquid.Bare.Types         as Bare
import           Language.Haskell.Liquid.Bare.Resolve       as Bare
import           Language.Haskell.Liquid.Bare.Expand        as Bare
import           Language.Haskell.Liquid.Bare.Misc         as Bare

import           Text.PrettyPrint.HughesPJ (text)
import qualified Control.Exception                 as Ex
import Control.Monad (forM, (<=<))
import Control.Monad.Extra (partitionM)



-------------------------------------------------------------------------------
makeMethodTypes :: Bool -> DEnv Ghc.Var LocSpecType -> [DataConP] -> [Ghc.CoreBind] -> [(Ghc.Var, MethodType LocSpecType)]
-------------------------------------------------------------------------------
makeMethodTypes allowTC (DEnv hm) cls cbs
  = [(x, MT (addCC allowTC x . fromRISig <$> methodType d x hm) (addCC allowTC x <$> classType (splitDictionary e) x)) | (d,e) <- ds, x <- grepMethods e]
    where
      grepMethods = filter GM.isMethod . freeVars mempty
      ds = filter (GM.isDictionary . fst) (concatMap unRec cbs)
      unRec (Ghc.Rec xes) = xes
      unRec (Ghc.NonRec x e) = [(x,e)]

      classType Nothing _ = Nothing
      classType (Just (d, ts, _)) x =
        case filter ((==d) . Ghc.dataConWorkId . dcpCon) cls of
          (di:_) ->
            (dcpLoc di `F.atLoc`) . subst (zip (dcpFreeTyVars di) ts) <$>
            L.lookup (mkSymbol x) (map (first lhNameToResolvedSymbol) $ dcpTyArgs di)
          _      -> Nothing

      methodType d x m = ihastype (M.lookup d m) x

      ihastype Nothing _    = Nothing
      ihastype (Just xts) x = M.lookup (mkSymbol x) xts

      mkSymbol x = F.dropSym 2 $ GM.simplesymbol x

      subst [] t = t
      subst ((a,ta):su) t = subsTyVarMeet' (a,ofType ta) (subst su t)

addCC :: Bool -> Ghc.Var -> LocSpecType -> LocSpecType
addCC allowTC var zz@(Loc l l' st0)
  = Loc l l'
  . addForall hst
  . mkArrow [] ps' []
  . makeCls cs'
  . mapExprReft (\_ -> F.applyCoSub coSub)
  . subts su
  $ st
  where
    hst           = ofType (Ghc.expandTypeSynonyms t0) :: SpecType
    t0            = Ghc.varType var
    tyvsmap       = case Bare.runMapTyVars allowTC t0 st err of
                          Left e  -> Ex.throw e
                          Right s -> Bare.vmap s
    su            = [(y, rTyVar x)               | (x, y) <- tyvsmap]
    su'           = [(y, RVar (rTyVar x) NoReft) | (x, y) <- tyvsmap] :: [(RTyVar, RSort)]
    coSub         = M.fromList [(F.symbol y, F.FObj (F.symbol x)) | (y, x) <- su]
    ps'           = fmap (subts su') <$> ps
    cs'           = [(F.dummySymbol, RApp c ts [] mempty) | (c, ts) <- cs ]
    (_,_,cs,_)    = bkUnivClass (F.notracepp "hs-spec" $ ofType (Ghc.expandTypeSynonyms t0) :: SpecType)
    (_,ps,_ ,st)  = bkUnivClass (F.notracepp "lq-spec" st0)

    makeCls c t  = foldr (uncurry rFun) t c
    err hsT lqT   = ErrMismatch (GM.fSrcSpan zz) (pprint var)
      (text "makeMethodTypes")
      (pprint $ Ghc.expandTypeSynonyms t0)
      (pprint $ toRSort st0)
      (Just (hsT, lqT))
      (Ghc.getSrcSpan var)

    addForall (RAllT v t r) tt@(RAllT v' _ _)
      | v == v'
      = tt
      | otherwise
      = RAllT (updateRTVar v) (addForall t tt) r
    addForall (RAllT v t r) t'
      = RAllT (updateRTVar v) (addForall t t') r
    addForall (RAllP _ t) t'
      = addForall t t'
    addForall _ (RAllP p t')
      = RAllP (fmap (subts su') p) t'
    addForall (RFun _ _ t1 t2 _) (RFun x i t1' t2' r)
      = RFun x i (addForall t1 t1') (addForall t2 t2') r
    addForall _ t
      = t


splitDictionary :: Ghc.CoreExpr -> Maybe (Ghc.Var, [Ghc.Type], [Ghc.Var])
splitDictionary = go [] []
  where
    go ts xs (Ghc.App e (Ghc.Tick _ a)) = go ts xs (Ghc.App e a)
    go ts xs (Ghc.App e (Ghc.Type t))   = go (t:ts) xs e
    go ts xs (Ghc.App e (Ghc.Var x))    = go ts (x:xs) e
    go ts xs (Ghc.Tick _ t) = go ts xs t
    go ts xs (Ghc.Var x) = Just (x, reverse ts, reverse xs)
    go _ _ _ = Nothing


-------------------------------------------------------------------------------
makeClasses :: Monad m => Bare.Env -> Bare.SigEnv -> ModName -> Bare.ModSpecs
            -> Bare.LookupT m ([DataConP], [(ModName, Ghc.Var, LocSpecType)])
-------------------------------------------------------------------------------
makeClasses env sigEnv myName specs = do
  mbZs <- forM classTcs $ \(name, cls, tc) ->
            mkClass env sigEnv myName name cls tc
  return . second mconcat . unzip . Mb.catMaybes $ mbZs
  where
    classTcs = [ (name, cls, tc) | (name, spec) <- M.toList specs
                                 , cls          <- Ms.classes spec
                                 , tc           <- Mb.maybeToList (classTc cls) ]
    classTc = Just . Bare.lookupGhcTyConLHName (reTyLookupEnv env) . btc_tc . rcName

mkClass :: Monad m => Bare.Env -> Bare.SigEnv -> ModName -> ModName -> RClass LocBareType -> Ghc.TyCon
        -> Bare.LookupT m (Maybe (DataConP, [(ModName, Ghc.Var, LocSpecType)]))
mkClass env sigEnv _myName name (RClass cc ss as ms)
  = Bare.failMaybe env name
  . mkClassE env sigEnv _myName name (RClass cc ss as ms)

mkClassE :: Monad m => Bare.Env -> Bare.SigEnv -> ModName -> ModName -> RClass LocBareType -> Ghc.TyCon
         -> Bare.LookupT m (DataConP, [(ModName, Ghc.Var, LocSpecType)])
mkClassE env sigEnv _myName name (RClass cc ss as ms) tc = do
    ss'    <- mapM (mkConstr   env sigEnv name) ss
    meths  <- mapM (makeMethod env sigEnv name) ms'
    let vts = [ (m, v, t) | (m, kv, t) <- meths, v <- Mb.maybeToList (plugSrc kv) ]
    let sts = [(val s, unClass $ val t) | (s, _) <- ms | (_, _, t) <- meths]
    let dcp = DataConP l dc αs [] (val <$> ss') (reverse sts) rt False (F.symbol name) l'
    return  $ F.notracepp msg (dcp, vts)
  where
    c      = btc_tc cc
    l      = loc  c
    l'     = locE c
    msg    = "MKCLASS: " ++ F.showpp (cc, as, αs)
    (dc:_) = Ghc.tyConDataCons tc
    αs     = bareRTyVar <$> as
    as'    = [rVar $ GM.symbolTyVar $ F.symbol a | a <- as ]
    ms'    = [ (s, rFun "" (RApp cc (flip RVar mempty <$> as) [] mempty) <$> t) | (s, t) <- ms]
    rt     = rCls tc as'

mkConstr :: Monad m => Bare.Env -> Bare.SigEnv -> ModName -> LocBareType -> Bare.LookupT m LocSpecType
mkConstr env sigEnv name = fmap (fmap dropUniv) . Bare.cookSpecTypeE env sigEnv name Bare.GenTV

   --FIXME: cleanup this code
unClass :: SpecType -> SpecType
unClass = snd . bkClass . thrd3 . bkUniv

makeMethod :: Monad m => Bare.Env -> Bare.SigEnv -> ModName -> (Located LHName, LocBareType)
           -> Bare.LookupT m (ModName, PlugTV Ghc.Var, LocSpecType)
makeMethod env sigEnv name (lx, bt) = (name, mbV,) <$> Bare.cookSpecTypeE env sigEnv name mbV bt
  where
    mbV = Bare.LqTV (Bare.lookupGhcIdLHName env lx)

-------------------------------------------------------------------------------
makeSpecDictionaries
  :: Monad m
  => Bare.Env
  -> Bare.SigEnv
  -> (ModName, Ms.BareSpec)
  -> [(ModName, Ms.BareSpec)]
  -> Bare.LookupT m ([RInstance LocBareType], DEnv Ghc.Var LocSpecType)
-------------------------------------------------------------------------------
makeSpecDictionaries env sigEnv spec0 specs = do
    (instances, specDicts) <- makeSpecDictionary env sigEnv spec0
    specsDicts <- traverse (return . snd <=< makeSpecDictionary env sigEnv) specs
    return (instances, dfromList $ specDicts ++ concat specsDicts)

makeSpecDictionary :: Monad m => Bare.Env -> Bare.SigEnv -> (ModName, Ms.BareSpec)
                   -> Bare.LookupT m ([RInstance LocBareType], [(Ghc.Var, M.HashMap F.Symbol (RISig LocSpecType))])
makeSpecDictionary env sigEnv (name, spec) = do
    let instances = Ms.rinstance spec
    resolved <-
        resolveDictionaries env <$>
          traverse (makeSpecDictionaryOne env sigEnv name) instances
    let updatedInstances =
          [ ri { riDictName = Just $ makeGHCLHNameLocatedFromId v }
          | (ri, (v, _)) <- zip instances resolved
          ]
    return (updatedInstances, resolved)

makeSpecDictionaryOne :: forall m . Monad m => Bare.Env -> Bare.SigEnv -> ModName
                      -> RInstance LocBareType
                      -> Bare.LookupT m (RInstance LocSpecType)
makeSpecDictionaryOne env sigEnv name (RI bt mDictName lbt xts)
  = fmap (F.notracepp "RI")
  $ RI
    <$> pure bt
    <*> pure mDictName
    <*> ts
    <*> traverse (bitraverse pure mkLSpecIType) xts
  where
    ts      = traverse mkTy' lbt
    rts     = concatMap (univs . val) <$> ts
    univs t = (\(RTVar tv _, _) -> tv) <$> as where (as, _, _) = bkUniv t

    mkTy' :: LocBareType -> Bare.LookupT m LocSpecType
    mkTy' = Bare.cookSpecType env sigEnv name Bare.GenTV
    mkTy :: LocBareType -> Bare.LookupT m LocSpecType
    mkTy = traverse (mapUnis tidy) <=< mkTy'

    mapUnis f t = do
      let (as, ps, t0) = bkUniv t
      as' <- f as
      return $ mkUnivs as' ps t0

    tidy vs = do
      (l,r) <- partitionM (\(RTVar tv _,_) -> rts >>= return . (tv `elem`)) vs
      return (l ++ r)

    mkLSpecIType :: RISig LocBareType -> Bare.LookupT m (RISig LocSpecType)
    mkLSpecIType t = traverse mkTy t

resolveDictionaries :: Bare.Env -> [RInstance LocSpecType]
                    -> [(Ghc.Var, M.HashMap F.Symbol (RISig LocSpecType))]
resolveDictionaries env = map $ \ri ->
    let !v = lookupDFun ri
     in (v, M.fromList $ first (getLHNameSymbol . val) <$> risigs ri)
  where
    lookupDFun (RI _ (Just ldict) _ _) = do
      Bare.lookupGhcIdLHName env ldict
    lookupDFun (RI c _ ts _) = do
       let tys = map (toType False . dropUniv . val) ts
       let tc = Bare.lookupGhcTyConLHName (reTyLookupEnv env) (btc_tc c)
       case Ghc.tyConClass_maybe tc of
          Nothing ->
            panic (Just $ GM.fSrcSpan $ btc_tc c) "type constructor does not refer to a type class"
          Just cls ->
            case Ghc.lookupInstEnv False (Bare.reInstEnvs env) cls tys of
              -- Is it ok to pick the first match?
              ((clsInst, _) : _, _, _) ->
                Ghc.is_dfun clsInst
              ([], _, _) ->
                panic (Just $ GM.fSrcSpan $ btc_tc c) "cannot find class instance"

dropUniv :: SpecType -> SpecType
dropUniv t = t' where (_,_,t') = bkUniv t


----------------------------------------------------------------------------------
makeDefaultMethods :: Bare.Env -> [(ModName, Ghc.Var, LocSpecType)]
                   -> [(ModName, Ghc.Var, LocSpecType)]
----------------------------------------------------------------------------------
makeDefaultMethods env mts = [ (mname, dm, t)
                                 | (mname, m, t) <- mts
                                 , Just dm <- [lookupDefaultVar env m]
                             ]

lookupDefaultVar :: Bare.Env -> Ghc.Var -> Maybe Ghc.Var
lookupDefaultVar env v =
    case Ghc.idDetails v of
      Ghc.ClassOpId cls _ -> do
        mdm <- lookup v (Ghc.classOpItems cls)
        (n, dmspec) <- mdm
        case dmspec of
          Ghc.VanillaDM -> Just $ lookupGhcIdLHName env (makeGHCLHNameLocated n)
          _ -> Nothing
      _ -> Nothing
