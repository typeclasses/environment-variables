{-# options_ghc -Wall -fno-warn-unused-imports #-}

{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables, PatternSynonyms #-}

import qualified Env (Lift, Var, integerDecimal, Readable, Name, pattern NameText, pattern VarNamed, Item (Item), productNames, sumNames)
import Env (Environment, pattern EnvironmentList, EnvFailure, Product, Sum, var, name, item, envs, read)
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
        (List.intercalate [""] $ List.map oneEnvOutput [base, app])
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

showEx :: Exception e => e -> TextBuilder.Builder
showEx = TextBuilder.fromString . displayException

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
    [ Demo app apiKey
    , Demo base apiKey
    , Demo app verbosity
    , Demo problem verbosity
    , Demo app $ (pure (,) * apiKey * apiSecret :: Env.Product (Text, Text))
    , Demo base $ (pure (,) * apiKey * apiSecret :: Env.Product (Text, Text))
    , Demo app $ (pure (,) * home * verbosity :: Env.Product (Text, Integer))
    , Demo base $ (pure (,) * home * verbosity :: Env.Product (Text, Integer))
    , Demo problem $ (pure (,) * home * verbosity :: Env.Product (Text, Integer))
    , Demo app $ (apiKey + apiSecret :: Env.Sum Text)
    , Demo problem $ (apiKey + apiSecret :: Env.Sum Text)
    ]

verbosity :: Env.Var Integer
verbosity = Env.integerDecimal "VERBOSITY"

apiKey :: Env.Name
apiKey = name "API_KEY"

apiSecret :: Env.Name
apiSecret = name "API_SECRET"

home :: Env.Name
home = name "HOME"
