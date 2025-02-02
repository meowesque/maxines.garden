module Parser.Error
  ( PError (..),
  )
where

import Data.SourcePos (SourcePos)
import Syntax.Token (Token)

data PError
  = PUnexpectedEof SourcePos
  | PUnexpected SourcePos
  | PUnexpectedToken Token [String]
  | PFailure String
  deriving (Show)