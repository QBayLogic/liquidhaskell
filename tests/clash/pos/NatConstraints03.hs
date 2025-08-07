{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NatConstraints03 where

import Numeric.Natural
import GHC.TypeNats

{-@ embed Natural as int @-}

data Index (n :: Nat) =
  Index { value :: Natural }

{-@ data Index n = Index { value :: {v:Natural | 0 <= v && v < n} } @-}

-- Nested tuples are translated to class dictionaries of type CTuple{n}, n being
-- the tuple size. They are defined in ghc-prim:GHC.Classes, see also
-- https://hackage.haskell.org/package/ghc-prim-0.13.0/docs/GHC-Classes.html#t:CTuple2
-- Note that the kind of the arguments to CTuple{n} is Constraint, meaning they
-- can directly be mentioned as an antecedent of the class. This is a
-- nice-to-have, since usages of nested constraint tuples are probably a code
-- smell.

decrement ::
  forall (n :: Nat) .
  ((2 <= n, 3 <= n), (2 <= n, 3 <= n)) =>
  Index n -> Index (n - 1)
decrement (Index i)
  | i == 0    = Index 0
  | otherwise = Index (i - 1)
