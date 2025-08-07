{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | Type variables and DataKind variables currently do not resolve at all in
-- the syntax for refinements.

module TypeNat02 where

import Numeric.Natural
import GHC.TypeNats

data Index (n :: Nat) =
  Index { value :: Natural }

{-@ data Index n = Index { value :: {v:Natural | v < n} } @-}
