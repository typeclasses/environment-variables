{-# options_ghc -Wall #-}

{-# language
    DeriveFunctor, DerivingStrategies, ExplicitNamespaces,
    GADTs, PatternSynonyms, RankNTypes, StandaloneDeriving
#-}

module Env.Types'
  (
    type Name, pattern NameText, pattern NameString,
    Parser,
    Default,
    NameWithDefault (..),
    Required (..),
    Optional (..),
    Var (..),
    Composite (..),
    NontrivialProduct (..),
    Product (..),
    Choice (..),
    NontrivialSum (..),
    Sum (..)
  )
  where

import Env.Name'

import Data.Text (Text)


-- ⭐  Parser  ⭐

-- | How to parse the text of an environment variable into some perhaps more meaningful value
type Parser a = Text -> Maybe a


-- ⭐  Default  ⭐

-- | Value to use instead of applying the parser if the name is not present in the environment
type Default a = a


-- ⭐  Name with default  ⭐

-- | A text environment variable, with a default value if not present
data NameWithDefault = NameWithDefault Name (Default Text)


-- ⭐  Required  ⭐

-- | A single required environment variable
data Required value = Required Name (Parser value)
  deriving Functor


-- ⭐  Optional  ⭐

-- | A single optional environment variable
data Optional value = Optional Name (Default value) (Parser value)
  deriving stock Functor


-- ⭐  Var  ⭐

-- | A single environment variable
data Var value = Var Name (Maybe (Default value)) (Parser value)

deriving stock instance Functor Var


-- ⭐  Composite  ⭐

-- | The product of multiplying two or more environment variables
data Composite value =
  forall arg.
    Composite
      (NontrivialProduct (arg -> value))
      (NontrivialProduct arg)

deriving stock instance Functor Composite


-- ⭐  Nontrivial product  ⭐

-- | The product of multiplying one or more environment variables
data NontrivialProduct value =
    UseOneVar (Var value)
  | UseManyVars (Composite value)
  deriving stock Functor


-- ⭐  Product  ⭐

-- | The product of multiplying any number of environment variables
data Product value =
    UseNoVars value
  | UseSomeVars (NontrivialProduct value)
  deriving stock Functor

instance Applicative Product where
    pure = UseNoVars

    UseNoVars   f <*> UseNoVars   x = UseNoVars (f x)
    UseSomeVars f <*> UseSomeVars x = UseSomeVars (UseManyVars (Composite f x))
    UseSomeVars f <*> UseNoVars   x = UseSomeVars (fmap ($ x) f)
    UseNoVars   f <*> UseSomeVars x = UseSomeVars (fmap f x)


-- ⭐  Choice  ⭐

-- | The sum of adding two or more environment variables
data Choice value = Choice (NontrivialSum value) (NontrivialSum value)
  deriving stock Functor

instance Semigroup (Choice a) where
    x <> y = Choice (ConsiderManyVars x) (ConsiderManyVars y)


-- ⭐  Nontrivial sum  ⭐

-- | The sum of adding one or more environment variables
data NontrivialSum value =
    ConsiderOneVar (Required value)
  | ConsiderManyVars (Choice value)
  deriving stock Functor

instance Semigroup (NontrivialSum a) where
    x <> y = ConsiderManyVars (Choice x y)


-- ⭐  Sum  ⭐

-- | The sum of adding any number of environment variables
data Sum value = ConsiderNoVars | ConsiderSomeVars (NontrivialSum value)
  deriving stock Functor

instance Semigroup (Sum a) where
    ConsiderNoVars <> x = x
    x <> ConsiderNoVars = x
    ConsiderSomeVars x <> ConsiderSomeVars y = ConsiderSomeVars (x <> y)

instance Monoid (Sum a) where
    mempty = ConsiderNoVars
