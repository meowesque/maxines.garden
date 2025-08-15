module Eval.Internal
  ( E (..)
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad (ap, liftM)
import Eval.Error (EError (..))
import Data.Text (Text)

data EState = EState
  { 
  }

newtype E a = E 
  { runE :: EState -> IO (Either EError (a, EState))
  }

instance Functor E where
  fmap = liftM

instance Applicative E where
  pure a = E $ pure . Right . (a,)
  (<*>) = ap

instance Monad E where
  E m >>= f = E \s -> m s >>= \case
    Left e -> pure $ Left e
    Right (a, s') -> runE (f a) s'

instance MonadIO E where
  liftIO m = E \s -> Right . (,s) <$> m

instance MonadError EError E where
  throwError = E . const . pure . Left 
  E m `catchError` f = E \s -> m s >>= \case
    Left e -> runE (f e) s
    right -> pure right