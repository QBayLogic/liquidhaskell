{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The subtype relation should follow naturally from type literals.

module TypeNat04 where

import Numeric.Natural
import GHC.TypeNats
import Data.Word (Word8)

data Index (n :: Nat) =
  Index { value :: Natural }

{-@ data Index n = Index { value :: {v:Natural | v < n} } @-}

indexFromIndex :: Index 5 -> Index 10
indexFromIndex = Index . value
