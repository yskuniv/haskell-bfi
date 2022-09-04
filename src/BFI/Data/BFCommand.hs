module BFI.Data.BFCommand
  ( BFCommand (..),
  )
where

data BFCommand
  = IncDataPtr
  | DecDataPtr
  | IncByte
  | DecByte
  | OutputByte
  | InputByte
  | CondJmpFwd
  | CondJmpBwd
  deriving (Eq)

instance Show BFCommand where
  show IncDataPtr = ">"
  show DecDataPtr = "<"
  show IncByte = "+"
  show DecByte = "-"
  show OutputByte = "."
  show InputByte = ","
  show CondJmpFwd = "["
  show CondJmpBwd = "]"
