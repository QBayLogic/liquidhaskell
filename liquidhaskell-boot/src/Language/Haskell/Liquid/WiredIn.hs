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

tupleNames :: [(Int, [LHName], [LHName])]
tupleNames =
  $(listE $ flip map [2..8::Int] $ \n ->
    let xs = map (logic . F.symbol . (("x_Tuple_"   <> show n) <>) . show) [1..n]
        fs = map (logic . F.symbol . (("fld_Tuple_" <> show n) <>) . show) [2..n]
        ln = lift n
     in tupE [ln, listE xs, listE fs]
  )


wiredInUniqueBound :: Word64
wiredInUniqueBound = $(sealUniqueCounter >> [| 0x8000_0000_0000_0000 |])
