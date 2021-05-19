{-# options_ghc -Wall #-}

{-# language LambdaCase, NoImplicitPrelude #-}

module Env.Sundries
  ( isPresent
  , integerDecimal
  ) where

import Env

import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Function ((.), ($), const)
import Data.Maybe (Maybe (..))
import Prelude (Integer)

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Read as LazyText

isPresent :: Name -> Optional Bool
isPresent = optional False . parse (const (Just True))

integerDecimal :: Parser Integer
integerDecimal = textRead $ LazyText.signed LazyText.decimal

textRead :: LazyText.Reader a -> Text -> Maybe a
textRead r = f . r . LazyText.fromStrict
  where
    f =
      \case
        Right (a, t) | LazyText.null t -> Just a
        _ -> Nothing
