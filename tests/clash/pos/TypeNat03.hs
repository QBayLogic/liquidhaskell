{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | LH should be able to show branches are unreachable due to conditions that
-- involve type-level naturals

module TypeNat03 where

import Numeric.Natural
import GHC.TypeNats
import Data.Word (Word8)

data Index (n :: Nat) =
  Index { value :: Natural }

{-@ data Index n = Index { value :: {v:Natural | v < n} } @-}

word8FromIndex :: Index 256 -> Word8
word8FromIndex Index { value }
  | 0 <= value && value < 256 = fromIntegral value
  | otherwise                 = error "unreachable"
