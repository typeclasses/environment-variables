{-# options_ghc -Wall -fno-warn-unused-imports #-}

{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, RankNTypes, ScopedTypeVariables, PatternSynonyms, ViewPatterns #-}

module Demo.Output where

import qualified Env
import Env.Environment (Environment, pattern EnvironmentList, Item (Item))
import Env.Ops ((*), (+))

import Demo.Environments
import Demo.Vars

import Control.Applicative (Applicative (..))
import Control.Exception (Exception (displayException))
import Data.Foldable (fold, toList)
import Data.Function ((.), ($), const)
import Data.Functor (Functor (..), fmap, (<$>))
import Data.Maybe (Maybe (..))
import Data.Semigroup (Semigroup, (<>))
import Data.Text (Text)
import Data.Validation (Validation, validation)
import Prelude (Integer, show, Show)
import System.IO (IO)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder

demoOutput :: LazyText.Text
demoOutput = TextBuilder.toLazyText $ fold $ List.map (<> "\n") lines
  where
    lines =
        (List.intercalate [""] $ List.map oneEnvOutput [base, app, problem])
        <> [""]
        <> List.map oneDemoOutput demos

oneEnvOutput :: DemoEnv -> [TextBuilder.Builder]
oneEnvOutput DemoEnv{ demoEnvName, demoEnvDescription, demoEnvironment = EnvironmentList items } =
    ("\"" <> TextBuilder.fromText demoEnvName <> "\" - " <> TextBuilder.fromText demoEnvDescription)
    : List.map (\(Item (Env.NameText x) y) -> TextBuilder.fromText x <> " = \"" <> TextBuilder.fromText y <> "\"") items

oneDemoOutput :: Demo -> TextBuilder.Builder
oneDemoOutput Demo{ demoEnv = DemoEnv{ demoEnvName, demoEnvironment }, demoVar } =
    "read " <> showDemoVar demoVar <> " " <> TextBuilder.fromText demoEnvName <> " = " <>
    validation Env.errorMessageBuilder (TextBuilder.fromString . show) (Env.read demoVar demoEnvironment)

class (Env.Readable a v, Show a) => DemoVar a v | v -> a
  where
    showDemoVar :: v -> TextBuilder.Builder

instance DemoVar Text Env.Name
  where
    showDemoVar (Env.NameText x) = TextBuilder.fromText x

instance Show a => DemoVar a (Env.Required a)
  where
    showDemoVar (Env.name -> x) = showDemoVar x

instance Show a => DemoVar a (Env.Optional a)
  where
    showDemoVar (Env.name -> x) = showDemoVar x

instance Show a => DemoVar a (Env.Product a)
  where
    showDemoVar = (\x -> "(" <> x <> ")") . fold . List.intersperse " * " . List.map (\(Env.NameText x) -> TextBuilder.fromText x) . toList . Env.nameSet

instance Show a => DemoVar a (Env.Sum a)
  where
    showDemoVar = (\x -> "(" <> x <> ")") . fold . List.intersperse " + " . List.map (\(Env.NameText x) -> TextBuilder.fromText x) . toList . Env.nameSet

data Demo = forall v a. (DemoVar a v) => Demo{ demoEnv :: DemoEnv, demoVar :: v }

demos :: [Demo]
demos =
  do
    V v <- demoVars
    e <- demoEnvs
    [ Demo e v ]

demoEnvs :: [DemoEnv]
demoEnvs = [ base, app, problem ]

data V = forall v a. DemoVar a v => V v

demoVars :: [V]
demoVars =
    [ V apiKey
    , V verbosity
    , V verbosityWithDefault
    , V apiCredentials
    , V homeAndVerbosity
    , V homeOrVerbosity
    , V verbosityOrHome
    , V homeOrUser
    , V homeIsPresent
    , V locale
    , V userAndApiCredentials
    ]
