module Data.Source
  ( Source (..),
    readSource,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO qualified as TextIO

data Source = Source
  { filePath :: FilePath,
    content :: !Text
  }
  deriving (Show, Eq)

readSource :: (MonadIO m) => FilePath -> m Source
readSource filePath = Source filePath <$> liftIO (TextIO.readFile filePath)