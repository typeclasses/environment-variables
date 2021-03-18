module IO where

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function
import Control.Exception (throwIO)
import Data.Validation

import Readable (Readable)
import qualified Readable as R

readVar :: (Readable v a, MonadIO m) => v -> m a
readVar = liftIO . (R.readVar >=> validation throwIO return)
