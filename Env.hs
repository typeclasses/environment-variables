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
    Readable (..), Context (..),
    -- * Var names
    Name,
    -- * What can go wrong
    EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..),
    -- * Environment
    Environment, pattern EnvironmentList, Item (..), getEnvironment
  ) where

import EnvData (Environment, pattern EnvironmentList, Item (..), getEnvironment)
import EnvFunctor (Context (..))
import IO (readVar)
import MultiVar (Multi)
import Name (Name)
import OneOptionalVar (Opt)
import OneRequiredVar (Var, var)
import Problems (EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..))
import Readable (Readable (..))
import VarConversions (optional, optionalMaybe, ToMulti (..))
