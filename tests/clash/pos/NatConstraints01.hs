{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NatConstraints01 where

import Numeric.Natural
import GHC.TypeNats

{-@ embed Natural as int @-}

data Index (n :: Nat) =
  Index { value :: Natural }

{-@ data Index n = Index { value :: {v:Natural | 0 <= v && v < n} } @-}

-- It would be good if LH understands that it does not need to reason about the
-- case of n == 1, in which case @0@ does not fit in @Index (n - 1)@.

decrement :: forall (n :: Nat) . 2 <= n => Index n -> Index (n - 1)
decrement (Index i)
  | i == 0    = Index 0
  | otherwise = Index (i - 1)
