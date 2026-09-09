{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.Liquid.WiredIn
       ( wildcardName
       , charX
       , xHead
       , xTail
       , tupleNames
       , listPredicate
       , listField
       , papp

       , wiredInUniqueBound
       ) where

import Prelude                                hiding (error)

import qualified Language.Fixpoint.Types as F
import           Language.Haskell.Liquid.WiredIn.TH
import           Language.Haskell.Liquid.Types.Names (LHName)
import           Language.Haskell.TH (listE, tupE)
import           Language.Haskell.TH.Syntax (lift)

import           Language.Haskell.Liquid.GHC.TypeRep ()
import           Data.Word (Word64)

wildcardName :: LHName
wildcardName = $(logic "_")

instance F.Binder LHName where
  wildcard = wildcardName

charX, xHead, xTail :: LHName
charX = $(logic "charX")
xHead = $(logic "head")
xTail = $(logic "tail")

tupleNames :: [(Int, [LHName], [LHName], [LHName])]
tupleNames =
  $(listE $ flip map [2..8::Int] $ \n ->
    let ps = map (logic . F.symbol . (("p_Tuple_"   <> show n <> "_") <>) . show) [2..n]
        xs = map (logic . F.symbol . (("x_Tuple_"   <> show n <> "_") <>) . show) [1..n]
        fs = map (logic . F.symbol . (("fld_Tuple_" <> show n <> "_") <>) . show) [2..n]
        ln = lift n
     in tupE [ln, listE ps, listE xs, listE fs]
  )

listPredicate, listField :: LHName
listPredicate = $(logic "p")
listField = $(logic "fldList")

papp :: [LHName]
papp = $(listE $ flip map [0..8::Int] $ \n -> logic (F.symbol $ "papp" <> show n))

wiredInUniqueBound :: Word64
wiredInUniqueBound = $(sealUniqueCounter >> [| 0x8000_0000_0000_0000 |])
