{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-@ LIQUID "--reflection" @-}

module BitVec00 where

import Data.Bits (shiftL)
import GHC.Num.Natural_LHAssumptions()
import GHC.TypeNats
import Language.Haskell.Liquid.ProofCombinators

{-@ exp2 :: {v: Int | v >= 0} -> Integer @-}
exp2 :: Int -> Integer
exp2 0 = 1
exp2 n = 2 * exp2 (n - 1)

{-@ reflect exp2 @-}

data BitVector (n :: Nat) =
  BitVector { value :: Natural }

{-@ data BitVector n = BitVector { value :: {v: Natural | v < exp2 100} } @-}
