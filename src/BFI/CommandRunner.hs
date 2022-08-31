module BFI.CommandRunner
  ( BFIAction,
    runBFIAction,
    runCommands,
  )
where

import BFI.BFMemory
  ( BFMemoryActionT,
    decByteOfMemory,
    decDataPtrOfMemory,
    getByteFromMemory,
    incByteOfMemory,
    incDataPtrOfMemory,
    runBFMemoryT,
    setByteToMemory,
  )
import BFI.Data.BFCommand (BFCommand (..))
import BFI.Utils.ByteIO (getByte, putByte)
import Control.Monad.Except
  ( ExceptT,
    MonadError (catchError, throwError),
    MonadIO (liftIO),
    MonadTrans (lift),
    runExceptT,
    when,
  )
import Data.Binary (Word8)

type BFIAction = ExceptT String (BFMemoryActionT IO)

runBFIAction :: BFIAction () -> (String -> IO ()) -> IO [Word8]
runBFIAction action handler =
  runBFMemoryT $
    runExceptT $
      action `catchError` (liftIO . handler)

runCommands :: [BFCommand] -> BFIAction ()
runCommands [] = return ()
runCommands (CondJmpFwd : cs) = do
  (lcs, rcs) <- splitCommandsByCorrespondingCondJmpBwd cs
  condRepeatRunCommands lcs
  runCommands rcs
runCommands (CondJmpBwd : _) = do
  throwError "unexpected ] found"
runCommands (c : cs) = do
  runCommand c
  runCommands cs

--

runCommand :: BFCommand -> BFIAction ()
runCommand IncDataPtr = do
  lift incDataPtrOfMemory
runCommand DecDataPtr = do
  lift decDataPtrOfMemory
runCommand IncByte = do
  lift incByteOfMemory
runCommand DecByte = do
  lift decByteOfMemory
runCommand OutputByte = do
  v <- lift getByteFromMemory
  liftIO $ putByte v
runCommand InputByte = do
  v <- liftIO getByte
  lift $ setByteToMemory v
runCommand _ = throwError "*** BUG ***"

condRepeatRunCommands :: [BFCommand] -> BFIAction ()
condRepeatRunCommands commands = do
  v <- lift getByteFromMemory
  when (v /= 0) $ do
    runCommands commands
    condRepeatRunCommands commands

splitCommandsByCorrespondingCondJmpBwd :: Monad m => [BFCommand] -> ExceptT String m ([BFCommand], [BFCommand])
splitCommandsByCorrespondingCondJmpBwd [] = throwError "corresponding ] not found"
splitCommandsByCorrespondingCondJmpBwd (CondJmpBwd : cs) =
  return ([], cs)
splitCommandsByCorrespondingCondJmpBwd (CondJmpFwd : cs) = do
  (lcs, rcs) <- splitCommandsByCorrespondingCondJmpBwd cs
  (lrcs, rrcs) <- splitCommandsByCorrespondingCondJmpBwd rcs
  return ((CondJmpFwd : lcs) ++ (CondJmpBwd : lrcs), rrcs)
splitCommandsByCorrespondingCondJmpBwd (c : cs) = do
  (lcs, rcs) <- splitCommandsByCorrespondingCondJmpBwd cs
  return (c : lcs, rcs)
