{-# options_ghc -Wall #-}

{-# language
    DeriveAnyClass, DeriveDataTypeable,
    DeriveGeneric, DerivingStrategies, LambdaCase,
    GeneralizedNewtypeDeriving, NoImplicitPrelude,
    OverloadedStrings, PatternSynonyms, ViewPatterns
#-}

module Env.Problems
  (
    Missing (..), Invalid (..), ToProductFailure (..),
    ProductFailure,  pattern ProductFailureList,
    Problem (..), oneProblemFailure, OneFailure (..),
    HasErrorMessage (..)
  ) where

import Env.Name

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
newtype ProductFailure = ProductFailure { productFailureMap :: Map Name Problem }
    deriving stock (Eq, Ord, Show)
    deriving newtype (Semigroup, Monoid)

---  🌟 Lifts 🌟  ---

class ToProductFailure a where
    toProductFailure :: a -> ProductFailure

instance ToProductFailure Missing where
    toProductFailure (Missing x) = ProductFailure (Map.singleton x VarMissing)

instance ToProductFailure Invalid where
    toProductFailure (Invalid x) = ProductFailure (Map.singleton x VarInvalid)

instance ToProductFailure OneFailure where
    toProductFailure (OneFailure x f) = ProductFailure (Map.singleton f x)

instance ToProductFailure ProductFailure where
    toProductFailure x = x


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
        ProductFailure xs | Map.null xs -> "No problem."
        ProductFailure xs | Map.size xs >= 2, all (== VarMissing) (Map.elems xs) ->
            "Missing environment variables: " <> nameListAnd (Map.keys xs) <> "."
        x -> f x
          where
            f = unwords . List.map errorMessageBuilder . productFailureToList
            unwords = fold . List.intersperse (TextBuilder.fromString " ")


---

pattern ProductFailureList :: [OneFailure] -> ProductFailure
pattern ProductFailureList xs <- (productFailureToList -> xs)
  where
    ProductFailureList = listToProductFailure
{-# COMPLETE ProductFailureList #-}

productFailureToList :: ProductFailure -> [OneFailure]
productFailureToList = List.map (\(n, p) -> OneFailure p n) . Map.toList . productFailureMap

listToProductFailure :: [OneFailure] -> ProductFailure
listToProductFailure = ProductFailure . Map.fromList . List.map (\(OneFailure p n) -> (n, p))

oneProblemFailure :: Problem -> Name -> ProductFailure
oneProblemFailure p x = ProductFailure (Map.singleton x p)

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
