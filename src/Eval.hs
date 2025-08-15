module Eval
  ( eval
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Error.Class (MonadError (..))
import Unit (Unit)
import Data.Phase (Parsed, Evaluated)
import Eval.Internal (E (..))
import Eval.Error (EError (..))

eval :: (MonadIO m, MonadError EError m) => m (Unit Evaluated)
eval = undefined 

eval' :: Unit Parsed -> E ()
eval' pUnit = undefined