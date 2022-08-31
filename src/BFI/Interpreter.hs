module BFI.Interpreter
  ( runBFFile,
    runProgram,
  )
where

import BFI.CommandRunner (runBFIAction, runCommands)
import BFI.Parser (parseProgram)
import System.IO (hPutStrLn, stderr)

runBFFile :: FilePath -> IO ()
runBFFile path = runProgram =<< readFile path

runProgram :: String -> IO ()
runProgram prog = do
  _ <- runBFIAction (runCommands $ parseProgram prog) $
    \msg -> hPutStrLn stderr $ "Error: " ++ msg
  return ()
