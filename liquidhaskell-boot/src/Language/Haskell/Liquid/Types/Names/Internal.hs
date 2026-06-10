{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Types.Names.Internal where

import qualified Data.Binary as B
import qualified Liquid.GHC.API as GHC

import Control.DeepSeq            (NFData)
import Data.Data                  (Data)
import Data.Hashable              (Hashable(..))
import Data.Maybe                 (isJust)
import Data.Word                  (Word64)
import Data.String                (fromString)
import GHC.Generics               (Generic)
import Language.Haskell.Liquid.GHC.Misc (showPpr)
import Language.Haskell.TH.Syntax (Quote, Exp, Lift (..), unsafeCodeCoerce)
import Language.Fixpoint.Types    (Symbol, symbolString, PPrint(..), pprint)
import Text.PrettyPrint.HughesPJ  (text)

isWiredIn :: GHC.Uniquable a => a -> Bool
isWiredIn = isJust . GHC.lookupKnownKeyName . GHC.getUnique

liftWiredInGhcName :: Quote m => GHC.Name -> m Exp
liftWiredInGhcName n | isWiredIn n =
  let u = GHC.unpkUnique $ GHC.getUnique n
  in [| fromJust $ GHC.lookupKnownKeyName $ uncurry GHC.mkUnique $(lift u) |]
liftWiredInGhcName n =
  error $ "Cannot lift non-wired-in GHC name " <> show n

data LHUnique
  -- | The canonical way to construct a 'GhcUnique' is to use 'getLHUnique'.
  -- This leaves room to fix things up in the case that future GHC versions
  -- un-wire-in names that LH considers wired in. Accordingly there is no
  -- 'LHUniquable' instance for 'GHC.Unique', since a LH mapping from ghc's
  -- names to 'LHUnique' would necessarily need to consider an actual name,
  -- rather than only a non-wired-in 'GHC.Unique' value.
  = GhcUnique {-# UNPACK #-} !GHC.UniqueClass {-# UNPACK #-} !GHC.UniqueId
  | LHUnique {-# UNPACK #-} !Word64
  deriving (Data, Generic)

-- | A name for an entity that does not exist in Haskell
--
-- For instance, this can be used to represent predicate aliases
-- or uninterpreted functions.
data LogicName =
    LogicName
       -- | Unqualified symbol
      !Symbol
        -- | Module where the entity was defined
      !GHC.ModuleName
        -- | If the named entity is the reflection of some Haskell name
      !(Maybe GHC.Name)
      !LHUnique
    | GeneratedLogicName !Symbol !LHUnique
  deriving (Data, Generic, Eq)

-- | A name whose procedence is known.
data LHResolvedName
    = LHRLogic !LogicName
    | LHRGHC !GHC.Name           -- ^ A name for an entity that exists in Haskell
    | LHRLocal !Symbol !LHUnique -- ^ A name for a local variable, e.g. one that is
                                 --   bound by a type alias.
    | -- | The index of a name in some environment
      --
      -- Before serializing names, they are converted to indices. The names
      -- themselves are kept in an environment or table that is serialized
      -- separately. This is to acommodate how GHC serializes its Names.
      LHRIndex Word
  deriving (Data, Generic, Eq, Ord)

-- | A resolved name, carrying along the 'Symbol' found by the parser.
data LHName
    = -- | In order to integrate the resolved names gradually, we keep the
      -- unresolved names.
      LHNResolved !LHResolvedName !Symbol
  deriving (Data, Generic)

-- | A name that is potentially unresolved.
data LHUnresolved
    = LHNUnresolved !LHNameSpace !Symbol
    | LHUGHC !GHC.Name
  deriving (Data, Generic, Eq, Ord)

data LHNameSpace
    = LHTcName LHThisModuleNameFlag       -- ^ Type constructors
    | LHDataConName LHThisModuleNameFlag  -- ^ Data constructors with procedence
    | LHVarName LHThisModuleNameFlag      -- ^ Variables with procedence
    | LHLogicNameBinder                   -- ^ Logic names (LHS)
    | LHLogicName                         -- ^ Logic names (RHS)
  deriving (Data, Generic, Eq, Ord, Show)

-- | Whether the name should be looked up in the current module only or in any
-- module
data LHThisModuleNameFlag = LHThisModuleNameF | LHAnyModuleNameF
  deriving (Data, Eq, Generic, Ord, Show)

instance Eq LHUnique where
  _ == _ = True

-- | An Eq instance that ignores the Symbol in resolved names
instance Eq LHName where
  LHNResolved n0 _ == LHNResolved n1 _ = n0 == n1

instance Ord LHUnique where
  _ `compare` _ = EQ

-- | An Ord instance that ignores the Symbol in resolved names
instance Ord LHName where
  compare (LHNResolved n0 _) (LHNResolved n1 _) = compare n0 n1

instance Ord LogicName where
  compare (LogicName s1 m1 _ _) (LogicName s2 m2 _ _) =
    case compare s1 s2 of
      EQ -> GHC.stableModuleNameCmp m1 m2
      x -> x
  compare LogicName{} GeneratedLogicName{} = LT
  compare GeneratedLogicName{} LogicName{} = GT
  compare (GeneratedLogicName s1 _) (GeneratedLogicName s2 _) = compare s1 s2

instance Lift LHUnique where
  liftTyped = unsafeCodeCoerce . lift
  lift (GhcUnique c u) =
    let uniq = GHC.mkUnique c u in
    if isWiredIn uniq
      then [| GhcUnique $(lift c) $(lift u) |]
      else error $ "Cannot lift non-wired-in GHC unique " <> show uniq
  lift (LHUnique u) =
    [| LHUnique $(lift u) |]

instance Lift LogicName where
  liftTyped = unsafeCodeCoerce . lift
  lift (LogicName s (GHC.moduleNameString -> m) maybeName u) =
    let n' = case maybeName of
          Nothing -> [| Nothing |]
          Just n  -> [| Just $(liftWiredInGhcName n) |]
    in [| LogicName $(lift s) (GHC.mkModuleName $(lift m)) $(n') $(lift u) |]
  lift (GeneratedLogicName s u) =
    [| GeneratedLogicName $(lift s) $(lift u) |]

instance Lift LHResolvedName where
  liftTyped = unsafeCodeCoerce . lift
  lift (LHRLogic n)   = [| LHRLogic $(lift n) |]
  lift (LHRGHC n)     = [| LHRGHC $(liftWiredInGhcName n) |]
  lift (LHRLocal _ _) = error "Cannot lift LHRLocal"
  lift (LHRIndex _)   = error "Cannot lift LHRIndex"

instance Lift LHName where
  liftTyped = unsafeCodeCoerce . lift
  lift (LHNResolved n s)   = [| LHNResolved $(lift n) $(lift s) |]

instance Hashable LHUnique where
  hashWithSalt salt _ = salt

-- | A Hashable instance that ignores the Symbol in resolved names
instance Hashable LHName where
  hashWithSalt s (LHNResolved n _) = hashWithSalt s n

instance Hashable LHUnresolved where
  hashWithSalt s (LHNUnresolved ns sym) =
    s `hashWithSalt` (0::Int) `hashWithSalt` ns `hashWithSalt` sym
  hashWithSalt s (LHUGHC name) =
    s `hashWithSalt` (1::Int) `hashWithSalt` GHC.getKey (GHC.nameUnique name)

instance Hashable LHNameSpace
instance Hashable LHThisModuleNameFlag

instance Hashable LHResolvedName where
  hashWithSalt s (LHRLogic n) = s `hashWithSalt` (0::Int) `hashWithSalt` n
  hashWithSalt s (LHRGHC n) =
    s `hashWithSalt` (1::Int) `hashWithSalt` GHC.getKey (GHC.nameUnique n)
  hashWithSalt s (LHRLocal n _) = s `hashWithSalt` (2::Int) `hashWithSalt` n
  hashWithSalt s (LHRIndex w) = s `hashWithSalt` (3::Int) `hashWithSalt` w

instance Hashable LogicName where
  hashWithSalt s (LogicName sym m _ _) =
        s `hashWithSalt` sym
          `hashWithSalt` GHC.moduleNameString m
  hashWithSalt s (GeneratedLogicName sym _) =
        s `hashWithSalt` sym

instance NFData LHUnique
instance NFData LHUnresolved
instance NFData LHNameSpace
instance NFData LHThisModuleNameFlag
instance NFData LHName
instance NFData LHResolvedName
instance NFData LogicName

instance B.Binary LHUnique
instance B.Binary LHNameSpace
instance B.Binary LHThisModuleNameFlag
instance B.Binary LHName
instance B.Binary LHResolvedName where
  get = do
    tag <- B.getWord8
    case tag of
      0 -> LHRLocal . fromString <$> B.get <*> B.get
      1 -> LHRIndex <$> B.get
      _ -> error "B.Binary: invalid tag for LHResolvedName"
  put (LHRLogic _n) = error "cannot serialize LHRLogic"
  put (LHRGHC _n) = error "cannot serialize LHRGHC"
  put (LHRLocal s _) = B.putWord8 0 >> B.put (symbolString s)
  put (LHRIndex n) = B.putWord8 1 >> B.put n

instance B.Binary LHUnresolved where
  get = LHNUnresolved <$> B.get <*> B.get
  put (LHNUnresolved ns n) = B.put ns >> B.put n
  put (LHUGHC _) = error "cannot serialize LHUGHC"

instance GHC.Binary LHUnique where
  get bh = do
    tag <- GHC.getByte bh
    case tag of
      0 -> GhcUnique <$> GHC.get bh <*> GHC.get bh
      1 -> LHUnique  <$> GHC.get bh
      _ -> error "GHC.Binary: invalid tag for LHUnique"
  put_ bh (GhcUnique c u) = GHC.putByte bh 0 >> GHC.put_ bh c >> GHC.put_ bh u
  put_ bh (LHUnique u)    = GHC.putByte bh 1 >> GHC.put_ bh u


instance GHC.Binary LHResolvedName where
  get bh = do
    tag <- GHC.getByte bh
    case tag of
      0 -> LHRLogic <$> GHC.get bh
      1 -> LHRGHC <$> GHC.get bh
      2 -> LHRLocal <$> (fromString <$> GHC.get bh) <*> GHC.get bh
      _ -> error "GHC.Binary: invalid tag for LHResolvedName"
  put_ bh (LHRLogic n) = GHC.putByte bh 0 >> GHC.put_ bh n
  put_ bh (LHRGHC n) = GHC.putByte bh 1 >> GHC.put_ bh n
  put_ bh (LHRLocal n u) = GHC.putByte bh 2 >> GHC.put_ bh (symbolString n) >> GHC.put_ bh u
  put_ _bh (LHRIndex _n) = error "GHC.Binary: cannot serialize LHRIndex"

instance GHC.Binary LogicName where
  get bh = do
    tag <- GHC.getByte bh
    case tag of
      0 -> LogicName . fromString <$> GHC.get bh <*> GHC.get bh <*> GHC.get bh <*> GHC.get bh
      1 -> GeneratedLogicName <$> (fromString <$> GHC.get bh) <*> GHC.get bh
      _ -> error "GHC.Binary: invalid tag for LogicName"
  put_ bh (LogicName s m r u) = do
    GHC.putByte bh 0
    GHC.put_ bh (symbolString s) >> GHC.put_ bh m >> GHC.put_ bh r >> GHC.put_ bh u
  put_ bh (GeneratedLogicName s u) = do
    GHC.putByte bh 1
    GHC.put_ bh (symbolString s) >> GHC.put_ bh u

instance PPrint LHName where
  pprintTidy _ (LHNResolved _ s) = pprint s

instance PPrint LHUnresolved where
  pprintTidy _ (LHNUnresolved _ name) = pprint name
  pprintTidy _ (LHUGHC name) = text $ showPpr name
