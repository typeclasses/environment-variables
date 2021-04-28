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
    var, Var,
    -- ** Optional
    optional, optionalMaybe, Opt,
    -- ** Multiple
    Product,
    Sum,
    -- ** Lifting
    Lift (..),
    -- ** Some particulars
    integerDecimal,
    -- * Using vars
    Readable (..), Context (..),
    -- * Var names
    Name, name, pattern NameText, pattern NameString,
    -- * What can go wrong
    EnvFailure, pattern EnvFailureList, OneEnvFailure (..), Problem (..),
    -- * Environment
    Environment, pattern EnvironmentList, Item (..), envs, item, getEnvironment,
    -- * Miscellanious accessors
    varName, pattern VarNamed, pattern OptNamed, productNames, sumNames,
    -- * Re-exports
    Text
  ) where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Exception (Exception (displayException))
import Data.Data (Data)
import Data.Either (Either (..))
import Data.Eq (Eq)
import Data.Foldable (fold)
import Data.Function ((.), ($))
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

name :: Text -> Name
name = NameText

pattern NameString :: String -> Name
pattern NameString s <- ((\(NameText t) -> Text.unpack t) -> s)
  where
    NameString s = NameText (Text.pack s)
{-# COMPLETE NameString #-}

---

-- | A single required environment variable.
data Var value = Var Name (Text -> Maybe value)
    deriving stock Functor

instance IsString (Var Text)
  where
    fromString x = Var (fromString x) Just

var ::
    Name -- ^ The name of the environment variable to read.
    -> (Text -> Maybe value) -- ^ How to parse the text into a value.
    -> Var value
var = Var

varName :: Var a -> Name
varName (Var x _) = x

pattern VarNamed :: Name -> Var value
pattern VarNamed x <- Var x _
{-# COMPLETE VarNamed #-}

-- | A single optional environment variable.
data Opt value =
    Opt
      Name -- ^ The name of the environment variable to read.
      value -- ^ A value to use instead of applying the parser if the name is not present in the environment.
      (Text -> Maybe value) -- ^ How to parse the text into a value.
    deriving stock Functor

instance IsString (Opt (Maybe Text))
  where
    fromString x = Opt (fromString x) Nothing (Just . Just)

pattern OptNamed :: Name -> Opt value
pattern OptNamed x <- Opt x _ _
{-# COMPLETE OptNamed #-}

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

productNames :: Product a -> Set Name
productNames =
  \case
    UseNoVars _ -> mempty
    UseOneVar (VarNamed x) -> Set.singleton x
    UseOneOpt (OptNamed x) -> Set.singleton x
    UseManyVars a b -> productNames a <> productNames b

---

data Sum a
  where
    ConsiderNoVars :: Sum a
    ConsiderOneVar :: Var a -> Sum a
    ConsiderManyVars :: Sum a -> Sum a -> Sum a

instance IsString (Sum Text)
  where
    fromString = ConsiderOneVar . fromString

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
    ConsiderOneVar (VarNamed x) -> Set.singleton x
    ConsiderManyVars a b -> sumNames a <> sumNames b

---

optional ::
    value -- ^ Default value to return when the variable is absent from the environment.
    -> Var value -- ^ A required environment variable.
    -> Opt value -- ^ An optional environment variable.
    --
    -- * Returns the default value when the variable is absent from the environment.
    -- * Succeeds or fails according to the 'Var' parser when the variable is present in the environment.
optional d (Var x f) = Opt x d f

optionalMaybe ::
    Var value -- ^ A required environment variable.
    -> Opt (Maybe value) -- ^ An optional environment variable.
    --
    -- * Returns a 'Nothing' value when the variable is absent from the environment.
    -- * Returns a 'Just' value when the variable is present in the environment.
optionalMaybe = optionalAlternative

optionalList :: Var a -> Opt [a]
optionalList = optionalAlternative

optionalAlternative :: Alternative f => Var a -> Opt (f a)
optionalAlternative (Var x f) = Opt x empty (fmap pure . f)

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
    read = \case
      UseNoVars x -> pure (Success x)
      UseOneVar v -> read v
      UseOneOpt v -> read v
      UseManyVars mf v -> pure (<*>) <*> read mf <*> read v

instance Readable (Sum v) [v]
  where
    read = \case
      ConsiderNoVars -> pure (Success [])
      ConsiderOneVar v -> read (optionalList v)
      ConsiderManyVars x y -> pure (liftA2 (++)) <*> read x <*> read y

---

{- | A /lift/ is a trivial function that converts from a smaller type to a more complex one. Since 'lift' is polymorphic in both domain and codomain, explicit type annotations are recommended. -}

class Lift b a where
    lift :: a -> b

instance Lift (Product a) (Var a) where
    lift = UseOneVar

instance Lift (Sum a) (Var a) where
    lift = ConsiderOneVar

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

instance Lift (Sum Text) Name where
    lift = lift . lift @(Var Text)

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

---

integerDecimal :: Name -> Var Integer
integerDecimal n = var n $ textRead $ LazyText.signed LazyText.decimal

textRead :: LazyText.Reader a -> Text -> Maybe a
textRead r = f . r . LazyText.fromStrict
  where
    f =
      \case
        Right (a, t) | LazyText.null t -> Just a
        _ -> Nothing
