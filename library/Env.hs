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
    Name, pattern NameText, pattern NameString, NameWithDefault,
    -- * Defining vars
    -- ** Basics
    text, parse, Parser, Required,
    -- ** Optional
    Optionalize (..), Default, optionalMaybe, Optional,
    -- ** Multiple
    Product,
    Sum,
    -- ** Classes
    Addend (..), IsProduct (..), IsVar (..), HasNameSet (..),
    -- * Using vars
    readOrFail, test, Readable (..), Context (..),
    -- * What can go wrong
    ProductFailure, pattern ProductFailureList, OneFailure (..), Problem (..), HasErrorMessage (..), Missing, Invalid,
    -- * Re-exports
    -- ** Text
    -- $text
    Text
  ) where

import Env.Environment
import Env.Problems'
import Env.Types'
import Env.Validation'

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Applicative (Applicative (pure, liftA2))
import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>>=))
import Control.Monad (MonadFail, fail)
import Data.Bifunctor (bimap)
import Data.Either (Either, either)
import Data.Function ((.), ($), id)
import Data.Functor (Functor (..), fmap)
import Data.Maybe (Maybe (..), fromMaybe, maybe)
import Data.Monoid (Monoid (mempty))
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Void (Void)
import System.IO (IO)
import Text.Show (Show, show)

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Environment as Sys

---

text :: Name -> Required Text
text x = Required x Just

parse :: Parser value -> Name -> Required value
parse parser name = Required name parser

---

class Applicative context => Context context where
    lookup :: Name -> context (Maybe Text)

instance Context IO where
    lookup (NameText x) = fmap (fmap Text.pack) $ Sys.lookupEnv $ Text.unpack x

instance Context ((->) Environment) where
    lookup n (EnvironmentMap m) = Map.lookup n m

---

class Optionalize required optional value
    | optional -> value
    , required -> optional
    , optional -> required
  where
    -- | Turns a required environment variable reader into one that returns the default value when the variable is absent from the environment
    optional :: Default value -> required -> optional

instance Optionalize Name NameWithDefault Text where
    optional def name = NameWithDefault name def

instance Optionalize (Required value) (Optional value) value where
    optional def (Required name parser) = Optional name def parser

---

-- | Returns a 'Nothing' value when the variable is absent from the environment. Returns a 'Just' value when the variable is present in the environment.
optionalMaybe :: Required value -> Optional (Maybe value)
optionalMaybe = optionalAlternative

optionalAlternative :: Alternative f => Required a -> Optional (f a)
optionalAlternative (Required x f) = Optional x empty (fmap pure . f)

---

readOrFail :: (Context m, MonadFail m, Readable value error var, HasErrorMessage error) => var -> m value
readOrFail v = read v >>= either (fail . errorMessageString) pure

test :: (Context m, Readable value error var, HasErrorMessage error, Show value) => var -> m Text
test v = fmap (either errorMessageText (Text.pack . show)) (read v)

{- | Type parameters:

* @var@ - The type of variable you want to read: 'Name', 'Required', 'Optional', or 'Product'.
* @value@ - What type of value is produced when an environment variable is successfully read.
* @context@ - Normally 'IO', but possibly @('Environment' ->)@ if you are reading from a mock environment. -}

class Readable value error var | var -> value, var -> error where
    read :: forall context. Context context =>
        var -> context (Either error value)

instance Readable Text Missing Name where
    read name = fmap f $ lookup name
      where
        f = maybe (Failure $ Missing name) Success

instance Readable Text Void NameWithDefault where
    read (NameWithDefault name def) = fmap f $ lookup name
      where
        f = Success . fromMaybe def

instance Readable value OneFailure (Required value) where
    read (Required name parse) = fmap (f . g) $ lookup name
      where
        f = (>>= (maybe (Failure $ fromInvalid $ Invalid name) Success . parse))
        g = maybe (Failure $ fromMissing $ Missing name) Success

instance Readable value Invalid (Optional value) where
    read (Optional name def parse) = fmap f $ lookup name
      where
        f = maybe (Success def) g
        g = maybe (Failure $ Invalid name) Success . parse

instance Readable value OneFailure (Var value) where
    read = \case
      Var name Nothing parse -> read (Required name parse)
      Var name (Just def) parse -> fmap (bimap fromInvalid id) $ read (Optional name def parse)

instance Readable value ProductFailure (NontrivialProduct value) where
    read = \case
      UseOneVar v -> fmap (bimap toProductFailure id) $ read v
      UseManyVars v -> read v

instance Readable value ProductFailure (Composite value) where
    read (Composite mf v) = apValidation <$> read mf <*> read v

instance Readable value ProductFailure (Product value) where
    read = \case
      UseNoVars x -> pure (Success x)
      UseSomeVars x -> read x

instance Readable value SumFailure (Choice value) where
    read = firstPossibility

instance Readable value SumFailure (NontrivialSum value) where
    read = firstPossibility

instance Readable value SumFailure (Sum value) where
    read = firstPossibility


---

-- | Environment variables that also support enumerating the full set of possibilities that they might have chosen
class Possibilities value var where
    possibilities :: forall context possibilities.
        (Context context, Alternative possibilities) =>
        var -> context (SumFailure, possibilities value)

instance Possibilities value (Choice value) where
    possibilities (Choice x y) = pure (liftA2 (<|>)) <*> possibilities x <*> possibilities y

instance Possibilities value (NontrivialSum value) where
    possibilities = \case
      ConsiderOneVar x -> fmap (either (\e -> (toSumFailure e, empty)) (\a -> (mempty, pure a))) $ read x
      ConsiderManyVars x -> possibilities x

instance Possibilities value (Sum value) where
    possibilities = \case
      ConsiderNoVars -> pure (mempty, empty)
      ConsiderSomeVars x -> possibilities x

firstPossibility :: forall context value var.
    (Context context, Possibilities value var, HasNameSet var) =>
    var -> context (Validation SumFailure value)
firstPossibility v = fmap (\(e, m) -> maybe (Failure e) Success m) (possibilities v)

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
instance HasNameSet Name where
    nameSet = Set.singleton
instance HasNameSet NameWithDefault where
    nameSet (NameWithDefault x _) = nameSet x
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

-- $text
-- See "Data.Text"
