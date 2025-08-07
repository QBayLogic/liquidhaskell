{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NatConstraints02 where

import Numeric.Natural
import GHC.TypeNats

{-@ embed Natural as int @-}

data Index (n :: Nat) =
  Index { value :: Natural }

{-@ data Index n = Index { value :: {v:Natural | 0 <= v && v < n} } @-}

-- If you write a constraint tuple on the outermost layer, GHC simply translates
-- them to separate Core arguments, so this should not require any special
-- treatment in comparison to simple constraints.

decrement :: forall (n :: Nat) . (2 <= n, 3 <= n) => Index n -> Index (n - 1)
decrement (Index i)
  | i == 0    = Index 0
  | otherwise = Index (i - 1)
