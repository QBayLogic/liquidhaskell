module GHC.Num.Natural_LHAssumptions() where

import GHC.Num.Natural
import GHC.Exts
import GHC.Types_LHAssumptions()
import GHC.Num_LHAssumptions()

{-@
assume GHC.Num.Natural.NS :: x:Word# -> {v: Natural | v = (x :: int) }

embed Natural as int
@-}
