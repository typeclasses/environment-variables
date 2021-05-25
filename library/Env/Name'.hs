{-# options_ghc -Wall #-}

{-# language
    DeriveAnyClass, DeriveDataTypeable, DeriveFunctor,
    DeriveGeneric, DerivingStrategies, ExplicitNamespaces,
    GeneralizedNewtypeDeriving, NoImplicitPrelude,
    PatternSynonyms, ViewPatterns
#-}

module Env.Name' (type Name, pattern NameText, pattern NameString) where

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Hashable (Hashable)
import Data.Monoid (Monoid)
import Data.Ord (Ord)
import Data.Semigroup (Semigroup)
import Data.String (IsString, String)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Text.Show (Show)

-- | The name of an environment variable
newtype Name = NameText Text
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass (Hashable)
    deriving newtype (IsString, Semigroup, Monoid)

pattern NameString :: String -> Name
pattern NameString s <- ((\(NameText t) -> unpack t) -> s)
  where
    NameString s = NameText (pack s)
{-# COMPLETE NameString #-}
