module Syntax.Token
  ( TokenClass (..),
    tcContent,
    Token (..),
  )
where

import Data.SourceSpan (SourceSpan)
import Data.Text (Text)
import Data.Text qualified as Text

data TokenClass
  = TcEof
  | -- | Special Left Angle
    -- | > ⟨
    TcSLAngle
  | -- | Special Right Angle
    -- | > ⟩
    TcSRAngle
  | -- | Special left bracket
    -- | > ⟦
    TcSLBracket
  | -- | Special right square bracket
    -- | > ⟧
    TcSRBracket
  | TcLParen
  | TcRParen
  | TcQuote
  | TcStrLit Text
  | TcIntLit Int
  | TcIdent Text
  | TcSpan Text
  | TcCodeblock Text
  deriving (Show, Eq)

tcContent :: TokenClass -> Text
tcContent TcEof = ""
tcContent TcSLAngle = "⟨"
tcContent TcSRAngle = "⟩"
tcContent TcSLBracket = "⟦"
tcContent TcSRBracket = "⟧"
tcContent TcLParen = "("
tcContent TcRParen = ")"
tcContent TcQuote = "'"
tcContent (TcStrLit a) = a
tcContent (TcIntLit a) = Text.pack $ show a
tcContent (TcIdent a) = a
tcContent (TcSpan a) = a
tcContent (TcCodeblock a) = a

data Token = Token
  { span :: SourceSpan,
    class_ :: !TokenClass
  }
  deriving (Show, Eq)
