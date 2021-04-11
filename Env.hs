module Env
  (
    -- * Defining vars
    -- ** Basics
    var, Var,
    -- ** Optional
    optional, optionalMaybe, Opt,
    -- ** Multiple
    Product,
    -- ** Lifting
    Lift (..),
    -- * Using vars
    Readable (..), Context (..),
    -- * Var names
    Name, pattern NameText, pattern NameString,
    -- * What can go wrong
    EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..),
    -- * Environment
    Environment, pattern EnvironmentList, Item (..), getEnvironment
  ) where

import EnvData (Environment, pattern EnvironmentList, Item (..), getEnvironment)
import EnvFunctor (Context (..))
import MultiVar (Product (..))
import Name (Name, pattern NameText, pattern NameString)
import Var (Var (..), Opt (..), var)
import Problems (EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..))
import Readable (Readable (..))
import VarConversions (optional, optionalMaybe)

import Data.Function ((.))
import Data.Maybe (Maybe (..))
import Data.Text (Text)

-- | A /lift/ is a trivial function that converts from a smaller type to a more complex one. Since 'lift' is polymorphic in both domain and codomain, explicit type annotations are recommended.

class Lift b a where
    lift :: a -> b

instance Lift (Product a) (Var a) where
    lift = OneVar

instance Lift (Product a) (Opt a) where
    lift = OneOpt

instance Lift (Var Text) Name where
    lift x = Var x Just

instance Lift (Opt (Maybe Text)) Name where
    lift x = Opt x Nothing (Just . Just)

instance Lift (Product Text) Name where
    lift = lift . lift @(Var Text)

instance Lift (Product (Maybe Text)) Name where
    lift = lift . lift @(Opt (Maybe Text))
