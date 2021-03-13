module BFI.Parser
  ( parseProgram,
  )
where

import BFI.Data.BFCommand (BFCommand (..))

parseProgram :: String -> [BFCommand]
parseProgram =
  foldr
    ( \ch ->
        case toBFCommand ch of
          Just cmd -> (cmd :)
          Nothing -> id
    )
    []

--

toBFCommand :: Char -> Maybe BFCommand
toBFCommand '>' = return IncDataPtr
toBFCommand '<' = return DecDataPtr
toBFCommand '+' = return IncData
toBFCommand '-' = return DecData
toBFCommand '.' = return OutputData
toBFCommand ',' = return InputData
toBFCommand '[' = return CondJmpFwd
toBFCommand ']' = return CondJmpBwd
toBFCommand _ = Nothing
