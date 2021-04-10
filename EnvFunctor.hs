module EnvFunctor where

import Control.Applicative (Applicative)
import Data.Function ((.))
import Data.Functor (fmap)
import Data.Maybe (Maybe)
import System.IO (IO)

import Data.Text (Text)
import qualified Data.Text as Text

import EnvData (EnvData, lookupEnvData)
import Name (Name, nameText)

import qualified System.Environment as Sys

class Applicative context => EnvFunctor context
  where
    lookupEnv :: Name -> context (Maybe Text)

instance EnvFunctor IO
  where
    lookupEnv =
        fmap (fmap Text.pack)
        . Sys.lookupEnv
        . Text.unpack
        . nameText

instance EnvFunctor ((->) EnvData)
  where
    lookupEnv = lookupEnvData
