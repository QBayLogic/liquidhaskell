{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | This module shows an example of a data type with a DataKind argument of
-- kind Nat. LiquidHaskell will accept such arguments, but they are otherwise
-- unusable.

module TypeNat00 where

import Numeric.Natural
import GHC.TypeNats

data Index (n :: Nat) =
  Index { value :: Natural }

{-@ data Index n = Index { value :: Natural } @-}
