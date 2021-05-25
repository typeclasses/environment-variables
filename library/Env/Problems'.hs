{-# options_ghc -Wall #-}

{-# language
    DeriveAnyClass, DeriveDataTypeable,
    DeriveGeneric, DerivingStrategies, LambdaCase,
    GeneralizedNewtypeDeriving, NoImplicitPrelude,
    OverloadedStrings, PatternSynonyms, ViewPatterns
#-}

module Env.Problems'
  (
    -- * Types
    Problem (..),
    OneFailure (..),
    Missing (..),
    Invalid (..),
    ProductFailure, pattern ProductFailureList,
    SumFailure, pattern SumFailureList,

    -- * Lifting
    ToProductFailure (..),
    ToSumFailure (..),
    FromInvalid (..),
    FromMissing (..),

    -- * Error messages
    HasErrorMessage (..)

  ) where

import Env.Name'

import Data.Data (Data)
import Data.Eq (Eq, (==))
import Data.Foldable (fold, all)
import Data.Function ((.))
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Monoid (Monoid)
import Data.Ord (Ord, (>=))
import Data.Semigroup (Semigroup, (<>))
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude (Enum, Bounded, error)
import Text.Show (Show)

import qualified Data.List as List
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Map.Strict as Map


---  🌟 Types 🌟  ---

data Missing = Missing Name

data Invalid = Invalid Name

-- | Things that can go wrong with a single environment variable
data Problem = VarMissing | VarInvalid
    deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
    deriving anyclass (Hashable)

-- | The name of a single problematic environment variable, and what is wrong with it
data OneFailure = OneFailure Problem Name
    deriving stock (Eq, Ord, Show)

-- | Some number of environment variables that all have problems
newtype ProductFailure = ProductFailureMap (Map Name Problem)
    deriving stock (Eq, Ord, Show)
    deriving newtype (Semigroup, Monoid)

newtype SumFailure = SumFailureMap (Map Name Problem)
    deriving stock (Eq, Ord, Show)
    deriving newtype (Semigroup, Monoid)


---  🌟 Pattern synonyms 🌟  ---

pattern ProductFailureList :: [OneFailure] -> ProductFailure
pattern ProductFailureList xs <- (failureMapToList -> xs) where
    ProductFailureList = failureListToMap
{-# COMPLETE ProductFailureList #-}

pattern SumFailureList :: [OneFailure] -> SumFailure
pattern SumFailureList xs <- (failureMapToList -> xs) where
    SumFailureList = failureListToMap
{-# COMPLETE SumFailureList #-}

class IsFailureMap a where
    toFailureMap :: a -> Map Name Problem
    fromFailureMap :: Map Name Problem -> a

instance IsFailureMap ProductFailure where
    toFailureMap (ProductFailureMap x) = x
    fromFailureMap x = ProductFailureMap x

instance IsFailureMap SumFailure where
    toFailureMap (SumFailureMap x) = x
    fromFailureMap x = SumFailureMap x

failureMapToList :: IsFailureMap a => a -> [OneFailure]
failureMapToList = List.map (\(n, p) -> OneFailure p n) . Map.toList . toFailureMap

failureListToMap :: IsFailureMap a => [OneFailure] -> a
failureListToMap = fromFailureMap . Map.fromList . List.map (\(OneFailure p n) -> (n, p))


---  🌟 Lifting from Invalid 🌟  ---

class FromInvalid a where
    fromInvalid :: Invalid -> a

instance FromInvalid Invalid where
    fromInvalid x = x

instance FromInvalid OneFailure where
    fromInvalid (Invalid x) = OneFailure VarInvalid x

instance FromInvalid ProductFailure where
    fromInvalid (Invalid x) = ProductFailureMap (Map.singleton x VarInvalid)


---  🌟 Lifting from Missing 🌟  ---

class FromMissing a where
    fromMissing :: Missing -> a

instance FromMissing Missing where
    fromMissing x = x

instance FromMissing OneFailure where
    fromMissing (Missing x) = OneFailure VarMissing x

instance FromMissing ProductFailure where
    fromMissing (Missing x) = ProductFailureMap (Map.singleton x VarMissing)


---  🌟 Lifting to ProductFailure 🌟  ---

class ToProductFailure a where
    toProductFailure :: a -> ProductFailure

instance ToProductFailure ProductFailure where
    toProductFailure x = x

instance ToProductFailure Missing where
    toProductFailure (Missing x) = ProductFailureMap (Map.singleton x VarMissing)

instance ToProductFailure Invalid where
    toProductFailure (Invalid x) = ProductFailureMap (Map.singleton x VarInvalid)

instance ToProductFailure OneFailure where
    toProductFailure (OneFailure x f) = ProductFailureMap (Map.singleton f x)


---  🌟 Lifting to SumFailure 🌟  ---

class ToSumFailure a where
    toSumFailure :: a -> SumFailure

instance ToSumFailure SumFailure where
    toSumFailure x = x

instance ToSumFailure Missing where
    toSumFailure (Missing x) = SumFailureMap (Map.singleton x VarMissing)

instance ToSumFailure Invalid where
    toSumFailure (Invalid x) = SumFailureMap (Map.singleton x VarInvalid)

instance ToSumFailure OneFailure where
    toSumFailure (OneFailure x f) = SumFailureMap (Map.singleton f x)


---  🌟 Error messages 🌟  ---

class HasErrorMessage a
  where
    errorMessageBuilder :: a -> TextBuilder.Builder

    errorMessageText :: a -> Text
    errorMessageText = LazyText.toStrict . TextBuilder.toLazyText . errorMessageBuilder

instance HasErrorMessage Missing
  where
    errorMessageBuilder (Missing (NameText x)) =
        "Environment variable" ! quote (TextBuilder.fromText x) ! "is missing."

instance HasErrorMessage Invalid
  where
    errorMessageBuilder (Invalid (NameText x)) =
        "Environment variable" ! quote (TextBuilder.fromText x) ! "has an invalid value."

instance HasErrorMessage OneFailure
  where
    errorMessageBuilder (OneFailure y x) =
        case y of
            VarMissing -> errorMessageBuilder (Missing x)
            VarInvalid -> errorMessageBuilder (Invalid x)

instance HasErrorMessage ProductFailure
  where
    errorMessageBuilder =
      \case
        ProductFailureList [] -> "No problem."
        ProductFailureList [x] -> errorMessageBuilder x
        ProductFailureMap xs | Map.size xs >= 2, all (== VarMissing) (Map.elems xs) ->
            "Missing environment variables: " <> nameListAnd (Map.keys xs) <> "."
        ProductFailureList xs -> unwords (List.map errorMessageBuilder xs)
          where
            unwords = fold . List.intersperse (TextBuilder.fromString " ")

instance HasErrorMessage SumFailure
  where
    errorMessageBuilder =
      \case
        SumFailureList [] -> "No environment variables were considered."
        SumFailureList [x] -> errorMessageBuilder x
        SumFailureMap xs | Map.size xs == 2, all (== VarMissing) (Map.elems xs) ->
            "An environment variable named either" ! nameListOr (Map.keys xs) ! "is required."
        SumFailureMap xs | all (== VarMissing) (Map.elems xs) -> "Missing a required environment variable named either " <> nameListOr (Map.keys xs) <> "."
        SumFailureMap xs | Map.size xs == 2 -> ("An environment variable named either" ! nameListOr (Map.keys xs) ! "is required. ") <> unwords (List.map f (Map.toList xs))
          where
            f (NameText x, VarInvalid) = quote (TextBuilder.fromText x) ! "is invalid."
            f (NameText x, VarMissing) = quote (TextBuilder.fromText x) ! "is missing."
            unwords :: [TextBuilder.Builder] -> TextBuilder.Builder
            unwords = fold . List.intersperse (TextBuilder.fromString " ")
        SumFailureMap xs -> "One of the following environment variables is required: " <> nameListOr (Map.keys xs) <> ". " <> unwords (List.map f (Map.toList xs))
          where
            f (NameText x, VarInvalid) = quote (TextBuilder.fromText x) ! "is invalid."
            f (NameText x, VarMissing) = quote (TextBuilder.fromText x) ! "is missing."
            unwords :: [TextBuilder.Builder] -> TextBuilder.Builder
            unwords = fold . List.intersperse (TextBuilder.fromString " ")

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

nameListOr :: [Name] -> TextBuilder.Builder
nameListOr =
  \case
    [] ->
        "Nothing"
    [NameText x] ->
        TextBuilder.fromText x
    [NameText x, NameText y] ->
        quote (TextBuilder.fromText x) ! "or" !
        quote (TextBuilder.fromText y)
    names -> f names
      where
        f [] = error "nameListOr: f: empty list"
        f [_] = error "nameListOr: f: list with one item"
        f [NameText x, NameText y] =
              quote (TextBuilder.fromText x) <> ", or " <>
              quote (TextBuilder.fromText y)
        f (NameText x : xs) = quote (TextBuilder.fromText x) <> ", " <> f xs

-- | Concatenate two strings together with a space in between.
(!) :: (Semigroup a, IsString a) => a -> a -> a
a ! b = a <> " " <> b

-- | Surround a string with slanted quotation marks.
quote :: (Semigroup a, IsString a) => a -> a
quote x = "‘" <> x <> "’"
