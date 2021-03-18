module EnvData where

import Name

import Data.Maybe
import Data.String
import Data.Text (Text)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

class EnvData a
  where
    lookupEnv :: Name -> a -> Maybe Text

instance EnvData (Map Name Text)
  where
    lookupEnv = Map.lookup

instance EnvData (HashMap Name Text)
  where
    lookupEnv = HashMap.lookup
