{-# options_ghc
    -Wall
    -fno-warn-name-shadowing
#-}

{-# language
    DeriveAnyClass, DeriveDataTypeable, DeriveFunctor,
    DeriveGeneric, DerivingStrategies, DerivingVia,
    FlexibleInstances, FunctionalDependencies,
    GADTs, InstanceSigs, LambdaCase,
    GeneralizedNewtypeDeriving, NoImplicitPrelude,
    OverloadedStrings, PatternSynonyms,
    ScopedTypeVariables, StandaloneDeriving,
    TypeApplications, ViewPatterns
#-}

module Env
  (
    -- * Defining vars
    -- ** Basics
    parse, Required,
    -- ** Optional
    optional, optionalMaybe, Optional, isPresent,
    -- ** Multiple
    Product,
    Sum,
    -- ** Classes
    Addend (..), IsProduct (..), IsVar (..), HasNameSet (..),
    -- * Using vars
    Readable (..), Context (..),
    -- * Var names
    Name, pattern NameText, pattern NameString,
    -- * What can go wrong
    EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..),
    -- * Environment
    Environment, pattern EnvironmentList, Item (..), envs, item, getEnvironment,
    -- * Miscellanious accessors
    varName,
    -- * Re-exports
    Text
  ) where

import Env.Name
import Env.Problems

import Control.Applicative (Alternative (..), Applicative (..))
import Data.Bool (Bool (True, False))
import Data.Data (Data)
import Data.Eq (Eq)
import Data.Function ((.), ($), const)
import Data.Functor (Functor (..), fmap)
import Data.Hashable (Hashable)
import Data.List ((++))
import Data.Map (Map)
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid (Monoid (mempty))
import Data.Ord (Ord)
import Data.Semigroup (Semigroup, (<>))
import Data.Set (Set)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Validation (Validation (Success, Failure), bindValidation)
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show)

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Environment as Sys

-- | A single required environment variable.
data Required value = Required Name (Text -> Maybe value)
    deriving stock Functor

-- | A single optional environment variable.
data Optional value =
    Optional
      Name -- ^ The name of the environment variable to read.
      value -- ^ A value to use instead of applying the parser if the name is not present in the environment.
      (Text -> Maybe value) -- ^ How to parse the text into a value.
    deriving stock Functor

data Var value =
    Var
      Name -- ^ The name of the environment variable to read.
      (Maybe value) -- ^ A value to use instead of applying the parser if the name is not present in the environment.
      (Text -> Maybe value) -- ^ How to parse the text into a value.
  deriving stock Functor

---

parse ::
    Name -- ^ The name of the environment variable to read.
    -> (Text -> Maybe value) -- ^ How to parse the text into a value.
    -> Required value
parse = Required

text :: Name -> Required Text
text x = Required x Just

varName :: Required a -> Name
varName (Required x _) = x

---

newtype Environment = EnvironmentMap (Map Name Text)
    deriving stock (Eq, Ord, Show, Data)
    deriving newtype (Semigroup, Monoid)

data Item = Item Name Text
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass (Hashable)

item :: Name -> Text -> Item
item = Item

pattern EnvironmentList :: [Item] -> Environment
pattern EnvironmentList xs <- (\(EnvironmentMap m) -> List.map (\(n, v) -> Item n v) (Map.toList m) -> xs)
  where
    EnvironmentList = EnvironmentMap . Map.fromList . List.map (\(Item n v) -> (n, v))
{-# COMPLETE EnvironmentList #-}

envs :: [Item] -> Environment
envs = EnvironmentList

{- | Reads the process's entire environment at once. -}

getEnvironment :: IO Environment
getEnvironment = fmap (EnvironmentList . List.map (\(n, v) -> Item (fromString n) (fromString v))) Sys.getEnvironment

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

{- | The product of multiplying two or more environment variables. -}

data Composite a = forall x. Composite (NontrivialProduct (x -> a)) (NontrivialProduct x)

deriving stock instance Functor Composite

---

{- | The product of multiplying one or more environment variables. -}

data NontrivialProduct a = UseOneVar (Var a) | UseManyVars (Composite a)

deriving stock instance Functor NontrivialProduct

---

{- | The product of multiplying any number of individual environment variables. -}

data Product a = UseNoVars a | UseSomeVars (NontrivialProduct a)

deriving stock instance Functor Product

instance Applicative Product
  where
    pure = UseNoVars

    (<*>) :: forall a b. Product (a -> b) -> Product a -> Product b
    UseNoVars f <*> UseNoVars x = UseNoVars (f x)
    UseSomeVars f <*> UseSomeVars x = UseSomeVars (UseManyVars (Composite f x))
    UseSomeVars f <*> UseNoVars x = UseSomeVars (fmap ($ x) f)
    UseNoVars f <*> UseSomeVars x = UseSomeVars (fmap f x)

---

data Sum a
  where
    ConsiderNoVars :: Sum a
    ConsiderOneVar :: Required a -> Sum a
    ConsiderManyVars :: Sum a -> Sum a -> Sum a

instance Functor Sum
  where
    fmap f = \case
      ConsiderNoVars -> ConsiderNoVars
      ConsiderOneVar x -> ConsiderOneVar (fmap f x)
      ConsiderManyVars x1 x2 -> ConsiderManyVars (fmap f x1) (fmap f x2)

instance Semigroup (Sum a)
  where
    ConsiderNoVars <> x = x
    x <> ConsiderNoVars = x
    x <> y = ConsiderManyVars x y

instance Monoid (Sum a)
  where
    mempty = ConsiderNoVars

---

optional ::
    value -- ^ Default value to return when the variable is absent from the environment.
    -> Required value -- ^ A required environment variable.
    -> Optional value -- ^ An optional environment variable.
    --
    -- * Returns the default value when the variable is absent from the environment.
    -- * Succeeds or fails according to the 'Required' parser when the variable is present in the environment.
optional d (Required x f) = Optional x d f

optionalMaybe ::
    Required value -- ^ A required environment variable.
    -> Optional (Maybe value) -- ^ An optional environment variable.
    --
    -- * Returns a 'Nothing' value when the variable is absent from the environment.
    -- * Returns a 'Just' value when the variable is present in the environment.
optionalMaybe = optionalAlternative

optionalList :: Required a -> Optional [a]
optionalList = optionalAlternative

optionalAlternative :: Alternative f => Required a -> Optional (f a)
optionalAlternative (Required x f) = Optional x empty (fmap pure . f)

isPresent :: Name -> Optional Bool
isPresent x = Optional x False (const (Just True))

---

{- | Type parameters:

* @var@ - The type of variable you want to read: 'Name', 'Required', 'Optional', or 'Product'.
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

instance Readable (Required value) value
  where
    read (Required name parse) =
        fmap (`bindValidation` (justOr VarInvalid name . parse)) $
            read name

instance Readable (Optional value) value
  where
    read (Optional name def parse) =
        fmap (maybe (Success def) (justOr VarInvalid name . parse)) $
            lookup name

instance Readable (Var value) value
  where
    read = \case
      Var name Nothing parse -> read (Required name parse)
      Var name (Just def) parse -> read (Optional name def parse)

instance Readable (NontrivialProduct value) value
  where
    read = \case
      UseOneVar v -> read v
      UseManyVars v -> read v

instance Readable (Composite value) value
  where
    read (Composite mf v) = pure (<*>) <*> read mf <*> read v

instance Readable (Product value) value
  where
    read = \case
      UseNoVars x -> pure (Success x)
      UseSomeVars x -> read x

instance Readable (Sum value) [value]
  where
    read = \case
      ConsiderNoVars -> pure (Success [])
      ConsiderOneVar v -> read (optionalList v)
      ConsiderManyVars x y -> pure (liftA2 (++)) <*> read x <*> read y

---

class Addend a v | v -> a where
    addend :: v -> Sum a
instance Addend a (Required a) where
    addend = ConsiderOneVar
instance Addend Text Name where
    addend = addend . text

class IsProduct a p | p -> a where
    prime :: Var a -> p
instance IsProduct a (NontrivialProduct a) where
    prime = UseOneVar
instance IsProduct a (Product a) where
    prime = UseSomeVars . UseOneVar

class IsVar a v | v -> a where
    name :: v -> Name
    var :: v -> Var a
instance IsVar a (Var a) where
    name (Var x _ _) = x
    var x = x
instance IsVar Text Name where
    name x = x
    var x = Var x Nothing Just
instance IsVar a (Required a) where
    name (Required x _) = x
    var (Required x f) = Var x Nothing f
instance IsVar a (Optional a) where
    name (Optional x _ _) = x
    var (Optional x d f) = Var x (Just d) f

class HasNameSet a where
    nameSet :: a -> Set Name
instance HasNameSet (Name) where
    nameSet = Set.singleton
instance HasNameSet (Var a) where
    nameSet = nameSet . name
instance HasNameSet (Composite a) where
    nameSet (Composite f x) = nameSet f <> nameSet x
instance HasNameSet (NontrivialProduct a) where
    nameSet = \case
        UseOneVar x -> nameSet x
        UseManyVars x -> nameSet x
instance HasNameSet (Product a) where
    nameSet = \case
        UseNoVars _ -> Set.empty
        UseSomeVars x -> nameSet x
instance HasNameSet (Sum a) where
    nameSet = \case
        ConsiderNoVars -> mempty
        ConsiderOneVar (name -> x) -> nameSet x
        ConsiderManyVars a b -> nameSet a <> nameSet b
