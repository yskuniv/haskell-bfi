module BFI.BFMemory
  ( BFMemoryActionT,
    BFMemoryAction,
    runBFMemoryT,
    runBFMemory,
    getByteFromMemory,
    setByteToMemory,
    incByteOfMemory,
    decByteOfMemory,
    incDataPtrOfMemory,
    decDataPtrOfMemory,
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

getByteFromMemory :: Monad m => BFMemoryActionT m Word8
getByteFromMemory = do
  ptr <- get
  lift $ readDataArray ptr

setByteToMemory :: Monad m => Word8 -> BFMemoryActionT m ()
setByteToMemory v = do
  ptr <- get
  lift $ writeDataArray ptr v

incByteOfMemory :: Monad m => BFMemoryActionT m ()
incByteOfMemory = do
  ptr <- get
  lift $ do
    v <- readDataArray ptr
    writeDataArray ptr (succ v)

decByteOfMemory :: Monad m => BFMemoryActionT m ()
decByteOfMemory = do
  ptr <- get
  lift $ do
    v <- readDataArray ptr
    writeDataArray ptr (pred v)

incDataPtrOfMemory :: Monad m => BFMemoryActionT m ()
incDataPtrOfMemory = modify succ

decDataPtrOfMemory :: Monad m => BFMemoryActionT m ()
decDataPtrOfMemory = modify pred
