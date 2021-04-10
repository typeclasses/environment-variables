module Env
  (
    -- * Defining vars
    -- ** Basics
    var, Var,
    -- ** Optional
    optional, optionalMaybe, Opt,
    -- ** Multiple
    Multi, ToMulti (..),
    -- * Using vars
    Readable (..), EnvFunctor (..),
    -- * Var names
    Name,
    -- * What can go wrong
    EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..),
    -- * Mock environments
    EnvData, pattern EnvList, Item (..), getEnvData
  ) where

import EnvData (EnvData, pattern EnvList, Item (..), getEnvData)
import EnvFunctor (EnvFunctor (..))
import IO (readVar)
import MultiVar (Multi)
import Name (Name)
import OneOptionalVar (Opt)
import OneRequiredVar (Var, var)
import Problems (EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..))
import Readable (Readable (..))
import VarConversions (optional, optionalMaybe, ToMulti (..))
