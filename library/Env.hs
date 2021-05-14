{-# options_ghc
    -Wall
    -fno-warn-unused-imports
    -fno-warn-name-shadowing
#-}

{-# language
    DeriveAnyClass, DeriveDataTypeable, DeriveFunctor,
    DeriveGeneric, DerivingStrategies, DerivingVia,
    FlexibleContexts,
    FlexibleInstances, FunctionalDependencies,
    GADTs, InstanceSigs, LambdaCase,
    GeneralizedNewtypeDeriving, NoImplicitPrelude,
    OverloadedStrings, PatternSynonyms, RankNTypes,
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
    Addend (..), IsProduct (..), IsVar (..),
    -- * Using vars
    Readable (..), Context (..),
    -- * Var names
    Name, pattern NameText, pattern NameString,
    -- * What can go wrong
    EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..),
    -- * Environment
    Environment, pattern EnvironmentList, Item (..), envs, item, getEnvironment,
    -- * Miscellanious accessors
    varName, pattern RequiredNamed, pattern OptionalNamed, productNames, sumNames,
    -- * Re-exports
    Text
  ) where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Exception (Exception (displayException))
import Data.Bool (Bool (True, False))
import Data.Data (Data)
import Data.Either (Either (..))
import Data.Eq (Eq)
import Data.Foldable (fold)
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
import Data.String (IsString (fromString), String)
import Data.Text (Text)
import Data.Validation (Validation (Success, Failure), bindValidation)
import GHC.Generics (Generic)
import Prelude (Enum, Bounded, Integer)
import System.IO (IO)
import Text.Show (Show)

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Read as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Environment as Sys

---

-- | The name of an environment variable.
newtype Name = NameText Text
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass (Hashable)
    deriving newtype (IsString, Semigroup, Monoid)

pattern NameString :: String -> Name
pattern NameString s <- ((\(NameText t) -> Text.unpack t) -> s)
  where
    NameString s = NameText (Text.pack s)
{-# COMPLETE NameString #-}

---

-- | A single required environment variable.
data Required value = Required Name (Text -> Maybe value)
    deriving stock Functor

parse ::
    Name -- ^ The name of the environment variable to read.
    -> (Text -> Maybe value) -- ^ How to parse the text into a value.
    -> Required value
parse = Required

text :: Name -> Required Text
text x = Required x Just

varName :: Required a -> Name
varName (Required x _) = x

pattern RequiredNamed :: Name -> Required value
pattern RequiredNamed x <- Required x _
{-# COMPLETE RequiredNamed #-}

-- | A single optional environment variable.
data Optional value =
    Optional
      Name -- ^ The name of the environment variable to read.
      value -- ^ A value to use instead of applying the parser if the name is not present in the environment.
      (Text -> Maybe value) -- ^ How to parse the text into a value.
    deriving stock Functor

pattern OptionalNamed :: Name -> Optional value
pattern OptionalNamed x <- Optional x _ _
{-# COMPLETE OptionalNamed #-}

---

data Var value =
    Var
      Name -- ^ The name of the environment variable to read.
      (Maybe value) -- ^ A value to use instead of applying the parser if the name is not present in the environment.
      (Text -> Maybe value) -- ^ How to parse the text into a value.
  deriving stock Functor

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

compositeNames :: Composite a -> Set Name
compositeNames (Composite f x) = nontrivialProductNames f <> nontrivialProductNames x

---

{- | The product of multiplying one or more environment variables. -}

data NontrivialProduct a = UseOneVar (Var a) | UseManyVars (Composite a)

deriving stock instance Functor NontrivialProduct

nontrivialProductNames :: NontrivialProduct a -> Set Name
nontrivialProductNames = \case
    UseOneVar (name -> x) -> Set.singleton x
    UseManyVars x -> compositeNames x

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

productNames :: Product a -> Set Name
productNames =
  \case
    UseNoVars _ -> Set.empty
    UseSomeVars x -> nontrivialProductNames x

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

sumNames :: Sum a -> Set Name
sumNames =
  \case
    ConsiderNoVars -> mempty
    ConsiderOneVar (RequiredNamed x) -> Set.singleton x
    ConsiderManyVars a b -> sumNames a <> sumNames b

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

---

-- | Things that can go wrong with a single environment variable.
data Problem = VarMissing | VarInvalid
    deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
    deriving anyclass (Hashable)

data OneEnvFailure = OneEnvFailure Name Problem
    deriving stock (Eq, Ord, Show)

newtype EnvFailure = EnvFailure { envFailureMap :: Map Name Problem }
    deriving stock (Eq, Ord, Show)
    deriving newtype (Semigroup, Monoid)

instance Exception OneEnvFailure
  where
    displayException (OneEnvFailure x y) = LazyText.unpack $ TextBuilder.toLazyText $ oneFailureMessage y x

instance Exception EnvFailure
  where
    displayException = LazyText.unpack . TextBuilder.toLazyText . envFailureMessage

pattern EnvFailureList :: [OneEnvFailure] -> EnvFailure
pattern EnvFailureList xs <- (envFailureToList -> xs)
  where
    EnvFailureList = listToEnvFailure
{-# COMPLETE EnvFailureList #-}

envFailureToList :: EnvFailure -> [OneEnvFailure]
envFailureToList = List.map (\(n, p) -> OneEnvFailure n p) . Map.toList . envFailureMap

listToEnvFailure :: [OneEnvFailure] -> EnvFailure
listToEnvFailure = EnvFailure . Map.fromList . List.map (\(OneEnvFailure n p) -> (n, p))

envFailureMessage :: EnvFailure -> TextBuilder.Builder
envFailureMessage = fold . List.intersperse (TextBuilder.fromString " ") . List.map (\(OneEnvFailure n p) -> oneFailureMessage p n) . envFailureToList

oneFailureMessage :: Problem -> Name -> TextBuilder.Builder
oneFailureMessage = \case VarMissing -> missingMessage; VarInvalid -> invalidMessage

oneProblemFailure :: Problem -> Name -> EnvFailure
oneProblemFailure p x = EnvFailure (Map.singleton x p)

-- | Error message to report that a single environment variable is missing.
missingMessage :: Name -> TextBuilder.Builder
missingMessage (NameText x) = "Environment variable" ! quote (TextBuilder.fromText x) ! "is missing."

-- | Error message to report that a single environment variable is present but invalid.
invalidMessage :: Name -> TextBuilder.Builder
invalidMessage (NameText x) = "Environment variable" ! quote (TextBuilder.fromText x) ! "has an invalid value."

-- | Concatenate two strings together with a space in between.
(!) :: (Semigroup a, IsString a) => a -> a -> a
a ! b = a <> " " <> b

-- | Surround a string with slanted quotation marks.
quote :: (Semigroup a, IsString a) => a -> a
quote x = "‘" <> x <> "’"
