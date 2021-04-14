{-# options_ghc -Wall -fno-warn-unused-imports #-}

{-# language FlexibleContexts, NoImplicitPrelude, OverloadedStrings #-}

import qualified Env (Lift, times)
import Env (Environment, EnvFailure, Product, var, name, item, envs, read)

import Control.Applicative (Applicative (..))
import Control.Exception (Exception (displayException))
import Data.Foldable (fold)
import Data.Function ((.), ($), const)
import Data.Functor (Functor (..), fmap, (<$>))
import Data.Maybe (Maybe (..))
import Data.Semigroup (Semigroup, (<>))
import Data.Text (Text)
import Data.Validation (Validation, validation)
import System.IO (IO)

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder

main :: IO ()
main = LazyText.putStr demoOutput

env1 :: Environment
env1 = envs [item "x" "a", item "z" "4"]

demoOutput :: LazyText.Text
demoOutput = TextBuilder.toLazyText $ fold $ List.intersperse "\n" $ List.map (<> "\n") $ demoParts

(*) :: Env.Lift (Product a) x => Product (a -> b) -> x -> Product b
(*) = Env.times

demoParts :: [TextBuilder.Builder]
demoParts = List.map showValidation
  [ read (name "x") env1
  , read (name "y") env1
  , read (var "x" trivialSuccess) env1
  , read (var "x" trivialFailure) env1
  , read (pure (<>) * name "x" * name "z") env1
  , read (pure (<>) * name "x" * name "y") env1
  , read (pure (<>) * name "a" * name "b") env1
  , read (pure (<>) * name "x" * var "z" trivialSuccess) env1
  , read (pure (<>) * name "x" * var "z" trivialFailure) env1
  , read (pure (<>) * var "x" trivialFailure * var "z" trivialFailure) env1
  , read (pure (<>) * var "y" trivialFailure * var "z" trivialFailure) env1
  ]

trivialSuccess, trivialFailure :: Text -> Maybe Text
trivialSuccess = Just
trivialFailure = const Nothing

showValidation :: Validation EnvFailure Text -> TextBuilder.Builder
showValidation = validation showEx TextBuilder.fromText

showEx :: Exception e => e -> TextBuilder.Builder
showEx = TextBuilder.fromString . displayException
