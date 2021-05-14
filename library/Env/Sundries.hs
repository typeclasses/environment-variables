{-# options_ghc -Wall #-}
{-# language LambdaCase, NoImplicitPrelude #-}

module Env.Sundries
  ( integerDecimal
  ) where

import Env

import Data.Either (Either (..))
import Data.Function ((.), ($))
import Data.Maybe (Maybe (..))
import Prelude (Integer)

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Read as LazyText

integerDecimal :: Name -> Required Integer
integerDecimal n = parse n $ textRead $ LazyText.signed LazyText.decimal

textRead :: LazyText.Reader a -> Text -> Maybe a
textRead r = f . r . LazyText.fromStrict
  where
    f =
      \case
        Right (a, t) | LazyText.null t -> Just a
        _ -> Nothing

