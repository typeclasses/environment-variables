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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified System.Environment as Env

data Item = Item Name Text
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass (Hashable)

newtype Environment = EnvironmentMap (Map Name Text)
    deriving stock (Eq, Ord, Show, Data)
    deriving newtype (Semigroup, Monoid)

lookup :: Name -> Environment -> Maybe Text
lookup n (EnvironmentMap m) = Map.lookup n m

pattern EnvironmentList :: [Item] -> Environment
pattern EnvironmentList xs <- (\(EnvironmentMap m) -> List.map (\(n, v) -> Item n v) (Map.toList m) -> xs)
  where
    EnvironmentList = EnvironmentMap . Map.fromList . List.map (\(Item n v) -> (n, v))

-- | Reads the process's entire environment at once.
getEnvironment :: IO Environment
getEnvironment = fmap (EnvironmentList . List.map (\(n, v) -> Item (fromString n) (fromString v))) Env.getEnvironment
