{-# options_ghc -Wall -fno-warn-unused-imports #-}

{-# language FlexibleContexts, NoImplicitPrelude, OverloadedStrings #-}

import qualified Env (Lift, Var, times, integerDecimal)
import Env (Environment, EnvFailure, Product, Sum, var, name, item, envs, read, zero, (||))

import Control.Applicative (Applicative (..))
import Control.Exception (Exception (displayException))
import Data.Foldable (fold)
import Data.Function ((.), ($), const)
import Data.Functor (Functor (..), fmap, (<$>))
import Data.Maybe (Maybe (..))
import Data.Semigroup (Semigroup, (<>))
import Data.Text (Text)
import Data.Validation (Validation, validation)
import Prelude (Integer, show)
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

baseEnv :: Environment
baseEnv =
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

appEnv :: Environment
appEnv =
  baseEnv <>
  envs
    [ item "VERBOSITY" "2"
    , item "API_KEY" "j91bD2ncr"
    , item "API_SECRET" "i12e9vnd32"
    ]

demoOutput :: LazyText.Text
demoOutput = TextBuilder.toLazyText $ fold $ List.intersperse "\n" $ List.map (<> "\n") $ demoParts

(*) :: Env.Lift (Product a) x => Product (a -> b) -> x -> Product b
(*) = Env.times

demoParts :: [TextBuilder.Builder]
demoParts =
  List.map (validation showEx TextBuilder.fromString)
    [ fmap show $ read (name "API_KEY") appEnv
    , fmap show $ read (name "API_KEY") baseEnv
    , fmap show $ read verbosity appEnv
    , fmap show $ read verbosity $ envs [item "VERBOSITY" "abc"]
    , fmap show $ read (pure (<>) * name "x" * name "z" :: Env.Product Text) env1
    , fmap show $ read (pure (<>) * name "x" * name "y" :: Env.Product Text) env1
    , fmap show $ read (pure (<>) * name "a" * name "b" :: Env.Product Text) env1
    , fmap show $ read (pure (<>) * name "x" * var "z" trivialSuccess :: Env.Product Text) env1
    , fmap show $ read (pure (<>) * name "x" * var "z" trivialFailure :: Env.Product Text) env1
    , fmap show $ read (pure (<>) * var "x" trivialFailure * var "z" trivialFailure :: Env.Product Text) env1
    , fmap show $ read (pure (<>) * var "y" trivialFailure * var "z" trivialFailure :: Env.Product Text) env1
    , fmap show $ read (name "x" || name "y" :: Env.Sum Text) env1
    , fmap show $ read (name "x" || name "z" :: Env.Sum Text) env1
    , fmap show $ read (name "x" || var "z" trivialFailure :: Env.Sum Text) env1
    , fmap show $ read (name "y" || var "z" trivialFailure :: Env.Sum Text) env1
    , fmap show $ read (name "a" || name "b" :: Env.Sum Text) env1
    ]

verbosity :: Env.Var Integer
verbosity = Env.integerDecimal "VERBOSITY"

trivialSuccess, trivialFailure :: Text -> Maybe Text
trivialSuccess = Just
trivialFailure = const Nothing

showEx :: Exception e => e -> TextBuilder.Builder
showEx = TextBuilder.fromString . displayException
