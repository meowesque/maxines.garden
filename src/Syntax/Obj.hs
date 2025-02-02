module Syntax.Obj
  ( ObjClass (..),
    Obj (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.SourceSpan (SourceSpan)

data ObjClass
  = OcList (NonEmpty Obj)
  | OcStrLit Text
  | OcIntLit Int
  | OcQuote Obj
  | OcIdent Text
  | OcDirective (NonEmpty Obj)
  | OcCodeblock Text
  | OcSpan Text
  deriving (Eq, Show)

data Obj = Obj
  { span :: SourceSpan
  , class_ :: !ObjClass
  }
  deriving (Eq, Show)