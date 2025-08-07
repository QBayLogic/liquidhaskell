{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-@ LIQUID "--reflection" @-}

module BitVec01 where

import Data.Bits (shiftL)
import GHC.Num.Natural_LHAssumptions()
import GHC.TypeNats
import Language.Haskell.Liquid.ProofCombinators

-- I would prefer this returns a 'Natural', but my tries to embed 'Natural' as a
-- type all failed so far. Even copying the exact approach of 'Integer' results
-- in @fromInteger@ remaining opaque somehow.

{-@ exp2 :: {v: Int | v >= 0} -> Integer @-}
exp2 :: Int -> Integer
exp2 0 = 1
exp2 n = 2 * exp2 (n - 1)

{-@ reflect exp2 @-}

-- When sticking with naturals as @BitVector@s encoding, so far the only
-- realistic option I've found is to do explicit inductive proofs over 2^n,
-- since reasoning about the value of 'exp2' requires explicitly unfolding the
-- definition with a usage.

{-@ exampleProof :: {exp2 0 == 1} @-}
exampleProof :: Proof
exampleProof = exp2 0 == 1 *** QED

data BitVector (n :: Nat) =
  BitVector { value :: Natural }

{-@ data BitVector n = BitVector { value :: {v: Natural | v < exp2 100} } @-}

-- This kind of definition is unfortunately not automatic.

example :: BitVector 100
example = BitVector 12345
