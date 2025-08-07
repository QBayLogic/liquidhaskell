{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NatConstraints04 where

import Numeric.Natural
import GHC.TypeNats

{-@ embed Natural as int @-}

-- Constraints can be defined via a Constraint-kinded type family. Internally
-- this is used by the typical constraints around type-level naturals, such as
-- (<=). They are eventually rooted in @Assert@ and @Compare@, where for example
-- Assert is defined as such:

-- type Assert :: Bool -> Constraint -> Constraint
-- type family Assert check errMsg where
--   Assert 'True _      = ()
--   Assert _     errMsg = errMsg

-- The following relies on a notion of the satisfiability of constraints that is
-- not necessarily defined as such by Haskell/GHC, so some fudging is required
-- to get to our end goal ("n >= 3 means that n is greater or equal to 3.")
--
-- There are three ground
