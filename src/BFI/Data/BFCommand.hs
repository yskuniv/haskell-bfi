module BFI.Data.BFCommand
  ( BFCommand (..),
  )
where

data BFCommand
  = IncDataPtr
  | DecDataPtr
  | IncData
  | DecData
  | OutputData
  | InputData
  | CondJmpFwd
  | CondJmpBwd
  deriving (Eq)

instance Show BFCommand where
  show IncDataPtr = ">"
  show DecDataPtr = "<"
  show IncData = "+"
  show DecData = "-"
  show OutputData = "."
  show InputData = ","
  show CondJmpFwd = "["
  show CondJmpBwd = "]"
