{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExplicitForAll #-}

{-@ LIQUID "--reflect" @-}
{-@ LIQUID "--allow-unsafe-constructors" @-}

module TypeNat05 where

import Numeric.Natural
import GHC.Num.Natural_LHAssumptions ()
import GHC.TypeNats
import Data.Word (Word8)

data Index (n :: Nat) =
  Index { value :: Natural }

{-@ reflect value @-}

{-@ Index :: forall (n :: Nat) .
      {i : Natural | i < n} ->
      {v : Index n | value v < n} @-}

data Indices a = Indices { l :: a }

{-@ reflect l @-}

type family SecretPlus (n :: Nat) (m :: Nat) :: Nat where
--   SecretPlus n _ = n

increment :: forall (n :: Nat) . Index n -> Indices [Index n]
{-@ increment ::
      forall (n :: Nat) .
      {v : _ | value v < n} ->
      {v : _ | l v != [] && value (head (l v)) < n } @-}
increment idx@(Index i) = Indices [Index i]
