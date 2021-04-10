module Env
  (
    -- * Defining vars
    -- ** Basics
    var, OneRequiredVar,
    -- ** Optional
    optional, optionalMaybe, OneOptionalVar,
    -- ** Multiple
    MultiVar, ToMulti (..),
    -- * Using vars
    Readable (..), EnvFunctor (..),
    -- * Var names
    Name,
    -- * What can go wrong
    EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..),
    -- * Mock environments
    EnvData, pattern EnvList, Item (..)
  ) where

import EnvData (EnvData, pattern EnvList, Item (..))
import EnvFunctor (EnvFunctor (..))
import IO (readVar)
import MultiVar (MultiVar)
import Name (Name)
import OneOptionalVar (OneOptionalVar)
import OneRequiredVar (OneRequiredVar, var)
import Problems (EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..))
import Readable (Readable (..))
import VarConversions (optional, optionalMaybe, ToMulti (..))
