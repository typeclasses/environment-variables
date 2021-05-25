{-# options_ghc -Wall #-}

{-# language
    DeriveFunctor, ExplicitNamespaces,
    LambdaCase, NoImplicitPrelude,
    PatternSynonyms, StandaloneDeriving
#-}

module Env.Validation'
  (
    -- * Aliases for Either
    type Validation, pattern Success, pattern Failure

    -- * Fun operations
  , apValidation
  ) where

import Data.Either (Either (..))
import Data.Semigroup (Semigroup (..))

type Validation e a = Either e a

pattern Failure :: a -> Either a b
pattern Success :: b -> Either a b
pattern Failure x = Left x
pattern Success x = Right x
{-# complete Failure, Success #-}

apValidation :: Semigroup e =>
    Validation e (a -> b) -> Validation e a
                          -> Validation e b
apValidation = (+)
  where
    Failure e1 + Failure e2 = Failure (e1 <> e2)
    Failure e  + Success _  = Failure e
    Success f  + Success x  = Success (f x)
    Success _  + Failure e  = Failure e
