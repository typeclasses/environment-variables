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
    -- * Var names
    Name, pattern NameText, pattern NameString,
    -- * Defining vars
    -- ** Basics
    parse, Parser, Required,
    -- ** Optional
    optional, Default, optionalMaybe, Optional, isPresent,
    -- ** Multiple
    Product,
    Sum,
    -- ** Classes
    Addend (..), IsProduct (..), IsVar (..), HasNameSet (..),
    -- * Using vars
    Readable (..), Context (..),
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
import Data.Foldable (foldMap)
import Data.Function ((.), ($), const)
import Data.Functor (Functor (..), fmap)
import Data.Hashable (Hashable)
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


---  🌟 Types for reading environment variables 🌟  ---

-- | How to parse the text of an environment variable into some perhaps more meaningful value
type Parser a = Text -> Maybe a

-- | Value to use instead of applying the parser if the name is not present in the environment
type Default a = a

-- | A single required environment variable
data Required value = Required Name (Parser value)

-- | A single optional environment variable
data Optional value = Optional Name (Default value) (Parser value)

data Var value = Var Name (Maybe (Default value)) (Parser value)

-- | The product of multiplying two or more environment variables
data Composite value =
    forall arg. Composite (NontrivialProduct (arg -> value)) (NontrivialProduct arg)

-- | The product of multiplying one or more environment variables
data NontrivialProduct value = UseOneVar (Var value) | UseManyVars (Composite value)

-- | The product of multiplying any number of individual environment variables
data Product value = UseNoVars value | UseSomeVars (NontrivialProduct value)

data Choice value = Choice (NontrivialSum value) (NontrivialSum value)

data NontrivialSum value = ConsiderOneVar (Required value) | ConsiderManyVars (Choice value)

data Sum value = ConsiderNoVars | ConsiderSomeVars (NontrivialSum value)


---  🌟 Functor instances for environment variable types 🌟  ---

deriving stock instance Functor Choice
deriving stock instance Functor Composite
deriving stock instance Functor NontrivialProduct
deriving stock instance Functor NontrivialSum
deriving stock instance Functor Optional
deriving stock instance Functor Product
deriving stock instance Functor Required
deriving stock instance Functor Sum
deriving stock instance Functor Var


---  🌟 Types for representing environments 🌟  ---

newtype Environment
  where
    EnvironmentMap :: Map Name Text -> Environment
    deriving stock (Eq, Ord, Show, Data)
    deriving newtype (Semigroup, Monoid)

data Item
  where
    Item :: Name -> Text -> Item
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass (Hashable)


---

parse :: Name -> Parser value -> Required value
parse = Required

text :: Name -> Required Text
text x = Required x Just

varName :: Required a -> Name
varName (Required x _) = x

---

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

multiply :: forall a b. Product (a -> b) -> Product a -> Product b
UseNoVars   f `multiply` UseNoVars   x = UseNoVars (f x)
UseSomeVars f `multiply` UseSomeVars x = UseSomeVars (UseManyVars (Composite f x))
UseSomeVars f `multiply` UseNoVars   x = UseSomeVars (fmap ($ x) f)
UseNoVars   f `multiply` UseSomeVars x = UseSomeVars (fmap f x)

instance Applicative Product
  where
    pure = UseNoVars
    (<*>) = multiply

---

instance Semigroup (Choice a)
  where
    x <> y = Choice (ConsiderManyVars x) (ConsiderManyVars y)

instance Semigroup (NontrivialSum a)
  where
    x <> y = ConsiderManyVars (Choice x y)

instance Semigroup (Sum a)
  where
    ConsiderNoVars <> x = x
    x <> ConsiderNoVars = x
    ConsiderSomeVars x <> ConsiderSomeVars y = ConsiderSomeVars (x <> y)

instance Monoid (Sum a)
  where
    mempty = ConsiderNoVars

---

-- | Returns the default value when the variable is absent from the environment. Succeeds or fails according to the 'Required' parser when the variable is present in the environment.
optional :: Default value -> Required value -> Optional value
optional d (Required x f) = Optional x d f

-- | Returns a 'Nothing' value when the variable is absent from the environment. Returns a 'Just' value when the variable is present in the environment.
optionalMaybe :: Required value -> Optional (Maybe value)
optionalMaybe = optionalAlternative

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

---

-- | Environment variables that also support enumerating the full set of possibilities that they might have chosen
class Readable var value => Possibilities var value
  where
    possibilities :: (Context context, Alternative possibilities) => var -> context (Validation EnvFailure (possibilities value))

instance Possibilities (Choice value) value
  where
    possibilities (Choice x y) = pure (liftA2 (<|>)) <*> possibilities x <*> possibilities y

instance Possibilities (NontrivialSum value) value
  where
    possibilities = \case
      ConsiderOneVar x -> read (optionalAlternative x)
      ConsiderManyVars x -> possibilities x

instance Possibilities (Sum value) value
  where
    possibilities = \case
      ConsiderNoVars -> pure (Success empty)
      ConsiderSomeVars x -> possibilities x

instance Readable (Choice        value) value where read = firstPossibility
instance Readable (NontrivialSum value) value where read = firstPossibility
instance Readable (Sum           value) value where read = firstPossibility

firstPossibility :: forall context var value. (Context context, Possibilities var value, HasNameSet var) => var -> context (Validation EnvFailure value)
firstPossibility v = fmap f (possibilities v)
  where
    f :: Validation EnvFailure [value] -> Validation EnvFailure value
    f = \case
      Failure e -> Failure e
      Success [] -> Failure ((foldMap (oneProblemFailure VarMissing) (Set.toList (nameSet v))))
      Success (x : _) -> Success x

---

class Addend a v | v -> a where
    addend :: v -> Sum a
instance Addend a (Required a) where
    addend = ConsiderSomeVars . ConsiderOneVar
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
instance HasNameSet () where
    nameSet _ = Set.empty
instance HasNameSet (Name) where
    nameSet = Set.singleton
instance HasNameSet (Var a) where
    nameSet = nameSet . name
instance HasNameSet (Required a) where
    nameSet = nameSet . name
instance HasNameSet (Optional a) where
    nameSet = nameSet . name
instance HasNameSet (Composite a) where
    nameSet (Composite f x) = nameSet f <> nameSet x
instance HasNameSet (NontrivialProduct a) where
    nameSet = \case
        UseOneVar x -> nameSet x
        UseManyVars x -> nameSet x
instance HasNameSet (Product a) where
    nameSet = \case
        UseNoVars _ -> nameSet ()
        UseSomeVars x -> nameSet x
instance HasNameSet (Choice a) where
    nameSet (Choice x y) = nameSet x <> nameSet y
instance HasNameSet (NontrivialSum a) where
    nameSet = \case
        ConsiderOneVar x -> nameSet x
        ConsiderManyVars x -> nameSet x
instance HasNameSet (Sum a) where
    nameSet = \case
        ConsiderNoVars -> nameSet ()
        ConsiderSomeVars x -> nameSet x
