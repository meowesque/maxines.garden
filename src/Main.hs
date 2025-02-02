module Main
  ( main,
  )
where

import Data.Function ((&))
import Data.Source (Source (..), readSource)
import Data.Text.Lazy.IO qualified as LazyText
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Parser (parseDocument)
import Parser.Internal
import Parser.Lexer

main :: IO ()
main = do
  setLocaleEncoding utf8
  source <- readSource "Garden/_.g"
  lexAll source & \case
    Left e -> print e
    Right a -> print a
  runP parseDocument (mkPState source) & \case
    Left e -> print e
    Right a -> print a
  pure ()
