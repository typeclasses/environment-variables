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

import EnvData (Environment, pattern EnvironmentMap, pattern EnvironmentList, Item (..), getEnvironment)
import Name (Name, pattern NameText, pattern NameString)
import Var (Var (..), Opt (..), var)
import Problems (EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..), oneProblemFailure)

import Control.Applicative (Applicative (..))
import Data.Function ((.), ($))
import Data.Functor (Functor (..), fmap)
import Data.Map (Map)
import Data.Maybe (Maybe (..), maybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Validation (Validation (Success, Failure), bindValidation)
import System.IO (IO)

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified System.Environment as Sys

---

class Applicative context => Context context
  where
    lookup :: Name -> context (Maybe Text)

instance Context IO
  where
    lookup (NameText x) = fmap (fmap Text.pack) $ Sys.lookupEnv $ Text.unpack x

instance Context ((->) Environment)
  where
    lookup n (EnvironmentMap m) = Map.lookup n m

---

{- | The product of multiplying any number of individual environment variables. Construct 'Product' values using 'lift', 'Applicative' combinators, and string overloading. -}

data Product a
  where
    UseNoVars :: a -> Product a
    UseOneVar :: Var a -> Product a
    UseOneOpt :: Opt a -> Product a
    UseManyVars :: Product (a -> b) -> Product a -> Product b

instance IsString (Product Text)
  where
    fromString = UseOneVar . fromString

instance IsString (Product (Maybe Text))
  where
    fromString = UseOneOpt . fromString

instance Functor Product
  where
    fmap f = \case
      UseNoVars x -> UseNoVars (f x)
      UseOneVar x -> UseOneVar (fmap f x)
      UseOneOpt x -> UseOneOpt (fmap f x)
      UseManyVars mf ma -> UseManyVars (fmap (f .) mf) ma

instance Applicative Product
  where
    pure = UseNoVars

    (<*>) :: forall a b. Product (a -> b) -> Product a -> Product b
    mf <*> (multi_a :: Product a) =
      case mf of
        UseNoVars (f :: a -> b) -> fmap f multi_a
        UseOneVar vf -> UseManyVars (UseOneVar vf) multi_a
        UseOneOpt vf -> UseManyVars (UseOneOpt vf) multi_a
        UseManyVars (multi_cab :: Product (c -> a -> b)) (v :: Product c) -> UseManyVars multi_cb v
          where
            multi_cb :: Product (c -> b)
            multi_cb = pure (\f c a -> f a c) <*> multi_cab <*> multi_a

---

optional ::
    value -- ^ Value to return when the variable is absent in the environment.
    -> Var value -- ^ A required environment variable.
    -> Opt value -- ^ An optional environment variable.
optional d (Var x f) = Opt x d f

optionalMaybe ::
    Var value -- ^ A required environment variable.
    -> Opt (Maybe value) -- ^ An optional environment variable. Returns a 'Just' value when the variable is present in the environment. Returns a 'Nothing' value when the variable is absent in the environment.
optionalMaybe (Var x f) = Opt x Nothing (fmap Just . f)

---

{- | Type parameters:

* @var@ - The type of variable you want to read: 'Name', 'Var', 'Opt', or 'Product'.
* @value@ - What type of value is produced when an environment variable is successfully read.
* @context@ - Normally 'IO', but possibly @('Environment' ->)@ if you are reading from a mock environment. -}

class Readable var value | var -> value
  where
    read :: Context context => var -> context (Validation EnvFailure value)

justOr :: Problem -> Name -> Maybe a -> Validation EnvFailure a
justOr x name = maybe (Failure (oneProblemFailure x name)) Success

instance Readable Name Text
  where
    read name = fmap (justOr VarMissing name) $ lookup name

instance Readable (Var a) a
  where
    read (Var name parse) =
        fmap (`bindValidation` (justOr VarInvalid name . parse)) $
            read name

instance Readable (Opt a) a
  where
    read (Opt name def parse) =
        fmap (maybe (Success def) (justOr VarInvalid name . parse)) $
            lookup name

instance Readable (Product v) v
  where
    read :: forall context value. Context context =>
        Product value -> context (Validation EnvFailure value)
    read = \case
      UseNoVars x -> pure (Success x)
      UseOneVar v -> read v
      UseOneOpt v -> read v
      UseManyVars mf v -> pure (<*>) <*> read mf <*> read v

---

{- | A /lift/ is a trivial function that converts from a smaller type to a more complex one. Since 'lift' is polymorphic in both domain and codomain, explicit type annotations are recommended. -}

class Lift b a where
    lift :: a -> b

instance Lift (Product a) (Var a) where
    lift = UseOneVar

instance Lift (Product a) (Opt a) where
    lift = UseOneOpt

instance Lift (Var Text) Name where
    lift x = Var x Just

instance Lift (Opt (Maybe Text)) Name where
    lift x = Opt x Nothing (Just . Just)

instance Lift (Product Text) Name where
    lift = lift . lift @(Var Text)

instance Lift (Product (Maybe Text)) Name where
    lift = lift . lift @(Opt (Maybe Text))
