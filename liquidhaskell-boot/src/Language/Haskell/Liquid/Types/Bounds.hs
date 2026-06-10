{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NamedFieldPuns     #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Haskell.Liquid.Types.Bounds (

    Bound(..),

    RBound, RRBound, RRBoundV, RRBoundBV,

    RBEnv, RRBEnv, RRBEnvV, RRBEnvBV,

    emapBoundM,
    mapBoundTy

    ) where

import Prelude hiding (error)
import Text.PrettyPrint.HughesPJ
import GHC.Generics
import Data.Hashable
import Data.Bifunctor as Bifunctor
import Data.Data
import qualified Data.Binary         as B
import Data.Traversable
import qualified Data.HashMap.Strict as M

import qualified Language.Fixpoint.Types as F
import Language.Haskell.Liquid.Types.RefType ()
import Language.Haskell.Liquid.Types.RType
import Language.Haskell.Liquid.Types.Types


data Bound b t e = Bound
  { bname   :: F.Located b        -- ^ The name of the bound
  , tyvars  :: [t]                -- ^ Type variables that appear in the bounds
  , bparams :: [(F.Located b, t)] -- ^ These are abstract refinements, for now
  , bargs   :: [(F.Located b, t)] -- ^ These are value variables
  , bbody   :: e                  -- ^ The body of the bound
  } deriving (Data, Generic, Functor, Foldable, Traversable)
  deriving B.Binary via Generically (Bound b t e)

type RBound           = RRBound RSort
type RRBound tv       = RRBoundV F.Symbol tv
type RRBoundV v tv    = RRBoundBV F.Symbol v tv
type RRBoundBV b v tv = Bound b tv (F.ExprBV b v)
type RBEnv            = M.HashMap LocSymbol RBound
type RRBEnv tv        = M.HashMap LocSymbol (RRBound tv)
type RRBEnvV v tv     = M.HashMap LocSymbol (RRBoundV v tv)
type RRBEnvBV b v tv  = M.HashMap b         (RRBoundBV b v tv)

emapBoundM
  :: Monad m
  => ([b] -> t0 -> m t1)
  -> ([b] -> e0 -> m e1)
  -> Bound b t0 e0
  -> m (Bound b t1 e1)
emapBoundM f g b = do
    tyvars <- mapM (f []) $ tyvars b
    (e1, bparams) <- mapAccumM (\e -> fmap (e,) . traverse (f e)) [] (bparams b)
    (e2, bargs) <- mapAccumM (\e -> fmap (e,) . traverse (f e)) e1 (bargs b)
    bbody <- g e2 (bbody b)
    return b{tyvars, bparams, bargs, bbody}

mapBoundTy :: (t0 -> t1) -> Bound b t0 e -> Bound b t1 e
mapBoundTy f Bound{..} = do
    Bound
      { tyvars = map f tyvars
      , bparams = map (fmap f) bparams
      , bargs = map (fmap f) bargs
      , ..
      }

instance Hashable b => Hashable (Bound b t e) where
  hashWithSalt i = hashWithSalt i . bname

instance Eq b => Eq (Bound b t e) where
  b1 == b2 = bname b1 == bname b2

instance (PPrint b, PPrint e, PPrint t) => (Show (Bound b t e)) where
  show = showpp


instance (PPrint b, PPrint e, PPrint t) => (PPrint (Bound b t e)) where
  pprintTidy k (Bound s vs ps ys e) = "bound" <+> pprintTidy k s <+>
                                      "forall" <+> pprintTidy k vs <+> "." <+>
                                      pprintTidy k (fst <$> ps) <+> "=" <+>
                                      ppBsyms k (fst <$> ys) <+> pprintTidy k e
    where
      ppBsyms _ [] = ""
      ppBsyms k' xs = "\\" <+> pprintTidy k' xs <+> "->"

instance Bifunctor (Bound b) where
  first  f (Bound s vs ps xs e) = Bound s (f <$> vs) (fmap f <$> ps) (fmap f <$> xs) e
  second = fmap
