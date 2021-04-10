module EnvData where

import Name (Name)

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor (fmap)
import Data.Hashable (Hashable)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid)
import Data.Ord (Ord)
import Data.Semigroup (Semigroup)
import Data.String (fromString)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show)

import qualified Data.List as List

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified System.Environment as Env

data Item = Item Name Text
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass (Hashable)

newtype EnvData = EnvData { envDataMap :: Map Name Text }
    deriving stock (Eq, Ord, Show, Data)
    deriving newtype (Semigroup, Monoid)

lookupEnvData :: Name -> EnvData -> Maybe Text
lookupEnvData n (EnvData m) = Map.lookup n m

pattern EnvList :: [Item] -> EnvData
pattern EnvList xs <- (envDataToList -> xs)
  where
    EnvList xs = listToEnvData xs

envDataToList :: EnvData -> [Item]
envDataToList = List.map (\(n, v) -> Item n v) . Map.toList . envDataMap

listToEnvData :: [Item] -> EnvData
listToEnvData = EnvData . Map.fromList . List.map (\(Item n v) -> (n, v))

-- | Reads the process's entire environment at once.
getEnvData :: IO EnvData
getEnvData = fmap (EnvList . List.map (\(n, v) -> Item (fromString n) (fromString v))) Env.getEnvironment
