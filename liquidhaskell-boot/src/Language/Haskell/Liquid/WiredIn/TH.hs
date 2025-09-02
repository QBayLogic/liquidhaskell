module Language.Haskell.Liquid.WiredIn.TH where

import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.Liquid.Types.Names
import qualified Language.Fixpoint.Types as F
import qualified Liquid.GHC.API as GHC
import Control.Monad (unless)

maxArity :: Arity
maxArity = 7

{-# NOINLINE uniqueCounterRef #-}
uniqueCounterRef :: IORef (Maybe Word64)
uniqueCounterRef = unsafePerformIO $ newIORef (Just 0)

nextUniqueWord :: Q Word64
nextUniqueWord = liftIO $ do
  u <- readIORef uniqueCounterRef
  case u of
    Nothing -> fail "Cannot generate more wired-in names after sealing."
    Just i  -> do
      writeIORef uniqueCounterRef (Just $ i + 1)
      return i

nextUnique :: Q LHUnique
nextUnique = LHUnique <$> nextUniqueWord

logic :: F.Symbol -> ExpQ
logic name = do
  u <- nextUnique
  lift $ LHNResolved (LHRLogic $ GeneratedLogicName name u) name

ghc :: GHC.Name -> ExpQ
ghc n = do
  unless (GHC.isKnownKeyName n) $
    fail $ "Cannot lift non-wired-in GHC name " <> show n
  return $ error ""

sealUniqueCounter :: Q Word64
sealUniqueCounter = liftIO $ do
  u <- readIORef uniqueCounterRef
  case u of
    Nothing -> fail "Cannot seal the unique counter twice."
    Just i  -> do
      writeIORef uniqueCounterRef Nothing
      return i
