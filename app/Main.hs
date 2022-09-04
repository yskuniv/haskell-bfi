module Main (main) where

import BFI.Interpreter (runBFFile)
import Options.Applicative
  ( argument,
    execParser,
    fullDesc,
    header,
    helper,
    info,
    metavar,
    progDesc,
    str,
    (<**>),
  )

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
            <> header "haskell-bfi - A brainf*ck interpreter implemented in haskell."
        )
