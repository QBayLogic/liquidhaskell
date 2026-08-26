module Language.Haskell.Liquid.Transforms.Simplify (simplifyBounds) where

import Prelude hiding (error)
import Language.Haskell.Liquid.Types.RType
import Language.Fixpoint.Types
import Language.Fixpoint.Types.Visitor
-- import Control.Applicative                 ((<$>))


simplifyBounds :: RTypeBV b v c tv (RReftBV b v) -> RTypeBV b v c tv (RReftBV b v)
simplifyBounds = fmap go
  where
    go x       = x { ur_reft = go' $ ur_reft x }
    -- OLD go' (Reft (v, rs)) = Reft(v, filter (not . isBoundLike) rs)
    go' (Reft (v, p)) = Reft(v, dropBoundLike p)

dropBoundLike :: ExprBV b v -> ExprBV b v
dropBoundLike p
  | isKvar p          = p
  | isBoundLikePred p = PTrue
  | otherwise         = p
  where
    isKvar            = not . null . kvarsExpr

isBoundLikePred :: ExprBV b v -> Bool
isBoundLikePred (PAnd ps) = simplifyLen <= length [p | p <- ps, isImp p ]
isBoundLikePred _         = False

isImp :: ExprBV b v -> Bool
isImp (PImp _ _) = True
isImp _          = False

-- OLD isBoundLike (RConc pred)  = isBoundLikePred pred
-- OLD isBoundLike (RKvar _ _)   = False


-- OLD moreThan 0 _            = True
-- OLD moreThan _ []           = False
-- OLD moreThan i (True  : xs) = moreThan (i-1) xs
-- OLD moreThan i (False : xs) = moreThan i xs

simplifyLen :: Int
simplifyLen = 5
