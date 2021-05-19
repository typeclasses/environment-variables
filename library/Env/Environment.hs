{-# options_ghc -Wall #-}

{-# language
    DeriveAnyClass, DeriveDataTypeable,
    DeriveGeneric, DerivingStrategies,
    GeneralizedNewtypeDeriving, NoImplicitPrelude,
    PatternSynonyms, ViewPatterns
#-}

module Env.Environment
  (
    Environment, pattern EnvironmentMap, pattern EnvironmentList,
    Item (..),
    getEnvironment
  ) where

import Env.Name

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor (Functor (..), fmap)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Monoid (Monoid)
import Data.Ord (Ord)
import Data.Semigroup (Semigroup)
import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Environment as Sys

newtype Environment = EnvironmentMap (Map Name Text)
    deriving stock (Eq, Ord, Show, Data)
    deriving newtype (Semigroup, Monoid)

data Item = Item Name Text
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass (Hashable)

pattern EnvironmentList :: [Item] -> Environment
pattern EnvironmentList xs <- (\(EnvironmentMap m) -> List.map (\(n, v) -> Item n v) (Map.toList m) -> xs)
  where
    EnvironmentList = EnvironmentMap . Map.fromList . List.map (\(Item n v) -> (n, v))
{-# COMPLETE EnvironmentList #-}

{- | Reads the process's entire environment at once. -}

getEnvironment :: IO Environment
getEnvironment = fmap (EnvironmentList . List.map (\(n, v) -> Item (fromString n) (fromString v))) Sys.getEnvironment
