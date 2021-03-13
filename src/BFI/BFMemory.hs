module BFI.BFMemory
  ( BFMemoryActionT,
    BFMemoryAction,
    runBFMemoryT,
    runBFMemory,
    getBFMemoryData,
    setBFMemoryData,
    incBFMemoryData,
    decBFMemoryData,
    incBFMemoryPtr,
    decBFMemoryPtr,
  )
where

import BFI.Utils.DataArray
  ( DataArrayActionT,
    readDataArray,
    runDataArrayT,
    writeDataArray,
  )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
  ( MonadState (get),
    MonadTrans (lift),
    StateT,
    execStateT,
    modify,
  )
import Data.Binary (Word8)

type BFMemoryActionT m = StateT Int (DataArrayActionT Word8 m)

type BFMemoryAction = BFMemoryActionT Identity

runBFMemoryT :: Monad m => BFMemoryActionT m a -> m [Word8]
runBFMemoryT action = runDataArrayT (execStateT action 0) 0

runBFMemory :: BFMemoryAction a -> [Word8]
runBFMemory action = runIdentity $ runBFMemoryT action

getBFMemoryData :: Monad m => BFMemoryActionT m Word8
getBFMemoryData = do
  ptr <- get
  lift $ readDataArray ptr

setBFMemoryData :: Monad m => Word8 -> BFMemoryActionT m ()
setBFMemoryData v = do
  ptr <- get
  lift $ writeDataArray ptr v

incBFMemoryData :: Monad m => BFMemoryActionT m ()
incBFMemoryData = do
  ptr <- get
  lift $ do
    v <- readDataArray ptr
    writeDataArray ptr (succ v)

decBFMemoryData :: Monad m => BFMemoryActionT m ()
decBFMemoryData = do
  ptr <- get
  lift $ do
    v <- readDataArray ptr
    writeDataArray ptr (pred v)

incBFMemoryPtr :: Monad m => BFMemoryActionT m ()
incBFMemoryPtr = modify succ

decBFMemoryPtr :: Monad m => BFMemoryActionT m ()
decBFMemoryPtr = modify pred
