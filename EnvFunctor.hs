module EnvFunctor where

import Control.Applicative
import Data.Function
import Data.Functor
import Data.Maybe
import Data.String
import System.IO

import Data.Text (Text)
import qualified Data.Text as Text

import EnvData (EnvData)
import qualified EnvData as EnvData
import Name

import qualified System.Environment as Sys

class Applicative f => EnvFunctor f
  where
    lookupEnv :: Name -> f (Maybe Text)

instance EnvFunctor IO
  where
    lookupEnv = fmap (fmap Text.pack) . Sys.lookupEnv . Text.unpack . nameText

instance EnvData a => EnvFunctor ((->) a)
  where
    lookupEnv = EnvData.lookupEnv
