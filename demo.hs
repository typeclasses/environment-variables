{-# options_ghc -Wall -fno-warn-unused-imports #-}

{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables, PatternSynonyms #-}

import qualified Env (Lift, Var, integerDecimal, Readable, Name, pattern NameText, pattern VarNamed, Item (Item), productNames, sumNames)
import Env (Environment, pattern EnvironmentList, EnvFailure, Product, Sum, var, name, item, envs, read, zero)
import Env.Ops ((*), (+))

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

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder

main :: IO ()
main = LazyText.putStr demoOutput

data DemoEnv = DemoEnv{ demoEnvName :: Text, demoEnvDescription :: Text, demoEnvironment :: Environment }

env1 :: DemoEnv
env1 = DemoEnv "env1" "Some nonsense" $ envs [item "x" "a", item "z" "4"]

base :: DemoEnv
base = DemoEnv{ demoEnvName, demoEnvDescription, demoEnvironment }
  where
    demoEnvName = "base"
    demoEnvDescription = "Base environment with some typical things"
    demoEnvironment =
      envs
        [ item "USER" "chris"
        , item "HOME" "/home/chris"
        , item "HOSTNAME" "cubby"
        , item "PWD" "/home/chris/environment-variables"
        , item "XDG_RUNTIME_DIR" "/user/run/1000"
        , item "SHELL" "/run/current-system/sw/bin/bash"
        , item "LANG" "en_US.UTF-8"
        , item "PATH" "/run/wrappers/bin:/run/current-system/sw/bin"
        ]

app :: DemoEnv
app = DemoEnv{ demoEnvName, demoEnvDescription, demoEnvironment }
  where
    demoEnvName = "app"
    demoEnvDescription = "App environment consisting of the base environment plus some things added specifically for the application"
    demoEnvironment =
      b <>
      envs
        [ item "VERBOSITY" "2"
        , item "API_KEY" "j91bD2ncr"
        , item "API_SECRET" "i12e9vnd32"
        ]
    DemoEnv{ demoEnvironment = b } = base

problem :: DemoEnv
problem = DemoEnv{ demoEnvName, demoEnvDescription, demoEnvironment }
  where
    demoEnvName = "problem"
    demoEnvDescription = "Problematic environment wherein everything has gone wrong"
    demoEnvironment = envs [item "VERBOSITY" "abc"]

demoOutput :: LazyText.Text
demoOutput = TextBuilder.toLazyText $ fold $ List.map (<> "\n") lines
  where
    lines =
        (List.intercalate [""] $ List.map oneEnvOutput [base, app, env1])
        <> [""]
        <> List.map oneDemoOutput demos

oneEnvOutput :: DemoEnv -> [TextBuilder.Builder]
oneEnvOutput DemoEnv{ demoEnvName, demoEnvDescription, demoEnvironment = EnvironmentList items } =
    ("\"" <> TextBuilder.fromText demoEnvName <> "\" - " <> TextBuilder.fromText demoEnvDescription)
    : List.map (\(Env.Item (Env.NameText x) y) -> TextBuilder.fromText x <> " = \"" <> TextBuilder.fromText y <> "\"") items

oneDemoOutput :: Demo -> TextBuilder.Builder
oneDemoOutput Demo{ demoEnv = DemoEnv{ demoEnvName, demoEnvironment }, demoVar } =
    "read " <> showDemoVar demoVar <> " " <> TextBuilder.fromText demoEnvName <> " = " <>
    validation showEx (TextBuilder.fromString . show) (Env.read demoVar demoEnvironment)

class (Env.Readable v a, Show a) => DemoVar v a | v -> a
  where
    showDemoVar :: v -> TextBuilder.Builder

instance DemoVar Env.Name Text
  where
    showDemoVar (Env.NameText x) = TextBuilder.fromText x

instance Show a => DemoVar (Env.Var a) a
  where
    showDemoVar (Env.VarNamed x) = showDemoVar x

instance Show a => DemoVar (Env.Product a) a
  where
    showDemoVar = (\x -> "(" <> x <> ")") . fold . List.intersperse " * " . List.map (\(Env.NameText x) -> TextBuilder.fromText x) . toList . Env.productNames

instance Show a => DemoVar (Env.Sum a) [a]
  where
    showDemoVar = (\x -> "(" <> x <> ")") . fold . List.intersperse " + " . List.map (\(Env.NameText x) -> TextBuilder.fromText x) . toList . Env.sumNames

data Demo = forall v a. (DemoVar v a) => Demo{ demoEnv :: DemoEnv, demoVar :: v }

demos :: [Demo]
demos =
  -- List.map (validation showEx TextBuilder.fromString)
    [ Demo app $ name "API_KEY"
    , Demo base $ name "API_KEY"
    , Demo app verbosity
    , Demo problem verbosity
    , Demo env1 $ (pure (<>) * name "x" * name "z" :: Env.Product Text)
    , Demo env1 $ (pure (<>) * name "x" * name "y" :: Env.Product Text)
    , Demo env1 $ (pure (<>) * name "a" * name "b" :: Env.Product Text)
    , Demo env1 $ (pure (<>) * name "x" * var "z" trivialSuccess :: Env.Product Text)
    , Demo env1 $ (pure (<>) * name "x" * var "z" trivialFailure :: Env.Product Text)
    , Demo env1 $ (pure (<>) * var "x" trivialFailure * var "z" trivialFailure :: Env.Product Text)
    , Demo env1 $ (pure (<>) * var "y" trivialFailure * var "z" trivialFailure :: Env.Product Text)
    , Demo env1 $ (name "x" + name "y" :: Env.Sum Text)
    , Demo env1 $ (name "x" + name "z" :: Env.Sum Text)
    , Demo env1 $ (name "x" + var "z" trivialFailure :: Env.Sum Text)
    , Demo env1 $ (name "y" + var "z" trivialFailure :: Env.Sum Text)
    , Demo env1 $ (name "a" + name "b" :: Env.Sum Text)
    ]

verbosity :: Env.Var Integer
verbosity = Env.integerDecimal "VERBOSITY"

trivialSuccess, trivialFailure :: Text -> Maybe Text
trivialSuccess = Just
trivialFailure = const Nothing

showEx :: Exception e => e -> TextBuilder.Builder
showEx = TextBuilder.fromString . displayException
