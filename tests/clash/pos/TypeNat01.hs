{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | In most cases it is nice, and sometimes required, to provide the kind
-- signature for declarations involving DataKind variables. LH could just grab
-- them from the original, or we could start supporting kind signatures in
-- refined declarations.

module TypeNat01 where

import Numeric.Natural
import GHC.TypeNats

data Index (n :: Nat) =
  Index { value :: Natural }

{-@ data Index (n :: Nat) = Index { value :: Natural } @-}
