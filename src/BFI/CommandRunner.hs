module BFI.CommandRunner
  ( BFIAction,
    runBFIAction,
    runCommands,
  )
where

import BFI.BFMemory
  ( BFMemoryActionT,
    decBFMemoryData,
    decBFMemoryPtr,
    getBFMemoryData,
    incBFMemoryData,
    incBFMemoryPtr,
    runBFMemoryT,
    setBFMemoryData,
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
  (lcs, rcs) <- splitByCondJmpBwd cs
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
  lift incBFMemoryPtr
runCommand DecDataPtr = do
  lift decBFMemoryPtr
runCommand IncData = do
  lift incBFMemoryData
runCommand DecData = do
  lift decBFMemoryData
runCommand OutputData = do
  v <- lift getBFMemoryData
  liftIO $ putByte v
runCommand InputData = do
  v <- liftIO getByte
  lift $ setBFMemoryData v
runCommand _ = throwError "*** BUG ***"

condRepeatRunCommands :: [BFCommand] -> BFIAction ()
condRepeatRunCommands commands = do
  v <- lift getBFMemoryData
  when (v /= 0) $ do
    runCommands commands
    condRepeatRunCommands commands

splitByCondJmpBwd :: Monad m => [BFCommand] -> ExceptT String m ([BFCommand], [BFCommand])
splitByCondJmpBwd [] = throwError "corresponding ] not found"
splitByCondJmpBwd (CondJmpBwd : cs) =
  return ([], cs)
splitByCondJmpBwd (CondJmpFwd : cs) = do
  (lcs, rcs) <- splitByCondJmpBwd cs
  (lrcs, rrcs) <- splitByCondJmpBwd rcs
  return (CondJmpFwd : lcs ++ [CondJmpBwd] ++ lrcs, rrcs)
splitByCondJmpBwd (c : cs) = do
  (lcs, rcs) <- splitByCondJmpBwd cs
  return (c : lcs, rcs)
