{-# options_ghc -Wall #-}

{-# language
    DeriveAnyClass, DeriveDataTypeable,
    DeriveGeneric, DerivingStrategies, LambdaCase,
    GeneralizedNewtypeDeriving, NoImplicitPrelude,
    OverloadedStrings, PatternSynonyms, ViewPatterns
#-}

module Env.Problems
  (
    EnvFailure,  pattern EnvFailureList,
    Problem (..), oneProblemFailure, OneFailure (..)
  ) where

import Env.Name

import Control.Exception (Exception (displayException))
import Data.Data (Data)
import Data.Eq (Eq, (==))
import Data.Foldable (fold, all)
import Data.Function ((.), ($))
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Monoid (Monoid)
import Data.Ord (Ord, (>=))
import Data.Semigroup (Semigroup, (<>))
import Data.String (IsString)
import GHC.Generics (Generic)
import Prelude (Enum, Bounded, error)
import Text.Show (Show)

import qualified Data.List as List
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Map.Strict as Map

-- | Things that can go wrong with a single environment variable.
data Problem = VarMissing | VarInvalid
    deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
    deriving anyclass (Hashable)

data OneFailure = OneFailure Name Problem
    deriving stock (Eq, Ord, Show)

newtype EnvFailure = EnvFailure { envFailureMap :: Map Name Problem }
    deriving stock (Eq, Ord, Show)
    deriving newtype (Semigroup, Monoid)

instance Exception OneFailure
  where
    displayException (OneFailure x y) = LazyText.unpack $ TextBuilder.toLazyText $ oneFailureMessage y x

instance Exception EnvFailure
  where
    displayException = LazyText.unpack . TextBuilder.toLazyText . envFailureMessage

pattern EnvFailureList :: [OneFailure] -> EnvFailure
pattern EnvFailureList xs <- (envFailureToList -> xs)
  where
    EnvFailureList = listToEnvFailure
{-# COMPLETE EnvFailureList #-}

envFailureToList :: EnvFailure -> [OneFailure]
envFailureToList = List.map (\(n, p) -> OneFailure n p) . Map.toList . envFailureMap

listToEnvFailure :: [OneFailure] -> EnvFailure
listToEnvFailure = EnvFailure . Map.fromList . List.map (\(OneFailure n p) -> (n, p))

envFailureMessage :: EnvFailure -> TextBuilder.Builder
envFailureMessage =
  \case
     EnvFailure xs | Map.null xs -> "No problem."
     EnvFailure xs | Map.size xs >= 2, all (== VarMissing) (Map.elems xs) ->
        "Missing environment variables: " <> nameListAnd (Map.keys xs) <> "."
     x -> f x
       where
         f = unwords . List.map message . envFailureToList
         message (OneFailure n p) = oneFailureMessage p n
         unwords = fold . List.intersperse (TextBuilder.fromString " ")

oneFailureMessage :: Problem -> Name -> TextBuilder.Builder
oneFailureMessage = \case VarMissing -> missingMessage; VarInvalid -> invalidMessage

oneProblemFailure :: Problem -> Name -> EnvFailure
oneProblemFailure p x = EnvFailure (Map.singleton x p)

-- | Error message to report that a single environment variable is missing.
missingMessage :: Name -> TextBuilder.Builder
missingMessage (NameText x) = "Environment variable" ! quote (TextBuilder.fromText x) ! "is missing."

-- | Error message to report that a single environment variable is present but invalid.
invalidMessage :: Name -> TextBuilder.Builder
invalidMessage (NameText x) = "Environment variable" ! quote (TextBuilder.fromText x) ! "has an invalid value."

nameListAnd :: [Name] -> TextBuilder.Builder
nameListAnd =
  \case
    [] ->
        "Nothing"
    [NameText x] ->
        TextBuilder.fromText x
    [NameText x, NameText y] ->
        quote (TextBuilder.fromText x) ! "and" !
        quote (TextBuilder.fromText y)
    names -> f names
      where
        f [] = error "nameListAnd: f: empty list"
        f [_] = error "nameListAnd: f: list with one item"
        f [NameText x, NameText y] =
              quote (TextBuilder.fromText x) <> ", and " <>
              quote (TextBuilder.fromText y)
        f (NameText x : xs) = quote (TextBuilder.fromText x) <> ", " <> f xs

-- | Concatenate two strings together with a space in between.
(!) :: (Semigroup a, IsString a) => a -> a -> a
a ! b = a <> " " <> b

-- | Surround a string with slanted quotation marks.
quote :: (Semigroup a, IsString a) => a -> a
quote x = "‘" <> x <> "’"
