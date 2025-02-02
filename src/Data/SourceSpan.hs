module Data.SourceSpan
  ( SourceSpan (..),
    mkSourceSpan,
  )
where

import Data.Source (Source)
import Data.SourcePos (SourcePos (..))

data SourceSpan = SourceSpan
  { source :: Source,
    -- | Line, Column
    begin :: !(Int, Int),
    -- | Line, Column
    end :: !(Int, Int)
  }
  deriving (Show, Eq)

instance Semigroup SourceSpan where
  l <> r = SourceSpan l.source l.begin r.end 

mkSourceSpan :: SourcePos -> SourcePos -> SourceSpan
mkSourceSpan l r =
  -- TODO(maxinedeandrade): Assert l < r && l.source == r.source
  SourceSpan l.source (l.line, l.column) (r.line, r.column)