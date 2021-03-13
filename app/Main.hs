module Main (main) where

import BFI.Interpreter (runBFFile)
import Options.Applicative

main :: IO ()
main = runBFFile =<< execParser opts
  where
    opts =
      info
        ( argument str (metavar "FILE")
            <**> helper
        )
        ( fullDesc
            <> progDesc "Run FILE"
            <> header "haskell-bfi - a BFI implemented in haskell"
        )
