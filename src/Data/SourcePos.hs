module Data.SourcePos
  ( SourcePos (..),
    next 
  )
where

import Data.Source (Source)

data SourcePos = SourcePos
  { source :: Source,
    line :: !Int,
    column :: !Int
  }
  deriving (Show, Eq)

next :: SourcePos -> Char -> SourcePos
next pos '\n' = pos {line = succ pos.line, column = 0}
next pos _ = pos {column = succ pos.column}