module EnvData where

import Name (Name)

import Data.Maybe (Maybe)
import Data.Text (Text)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

class EnvData mockEnvironment
  where
    lookupEnvData :: Name -> mockEnvironment -> Maybe Text

instance EnvData (Map Name Text)
  where
    lookupEnvData = Map.lookup

instance EnvData (HashMap Name Text)
  where
    lookupEnvData = HashMap.lookup
