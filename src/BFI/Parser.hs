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
toBFCommand '+' = return IncByte
toBFCommand '-' = return DecByte
toBFCommand '.' = return OutputByte
toBFCommand ',' = return InputByte
toBFCommand '[' = return CondJmpFwd
toBFCommand ']' = return CondJmpBwd
toBFCommand _ = Nothing
