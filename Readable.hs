module Readable (Readable (readVar)) where

import Data.Function
import Data.Functor
import Data.Functor.Compose
import Data.Maybe
import Data.String
import Data.Validation

import qualified Control.Applicative.Free.Final as Free

import Data.Text (Text)
import qualified Data.Text as Text

import EnvFunctor
import MultiVar (MultiVar (..))
import Name
import OneOptionalVar (OneOptionalVar (..))
import OneRequiredVar (OneRequiredVar (..))
import OneVar (OneVar)
import qualified OneVar
import Problems

class Readable v a | v -> a
  where
    readVar :: EnvFunctor f => v -> f (Result a)

type Result a = Validation EnvFailure a

justOr :: Problem -> Name -> Maybe a -> Result a
justOr x name = maybe (Failure (oneProblemFailure x name)) Success

justOrMissing :: Name -> Maybe a -> Result a
justOrMissing = justOr VarMissing

justOrInvalid :: Name -> Maybe a -> Result a
justOrInvalid = justOr VarInvalid

instance Readable Name Text
  where
    readVar name = justOrMissing name <$> lookupEnv name

instance Readable (OneRequiredVar a) a
  where
    readVar (OneRequiredVar name parse) =
      (`bindValidation` (justOrInvalid name . parse)) <$> readVar name

instance Readable (OneOptionalVar a) a
  where
    readVar (OneOptionalVar name def parse) =
        maybe (Success def) (justOrInvalid name . parse) <$> lookupEnv name

instance Readable (OneVar a) a
  where
    readVar =
      \case
        OneVar.Required n p -> readVar (OneRequiredVar n p)
        OneVar.Optional n d p -> readVar (OneOptionalVar n d p)

instance Readable (MultiVar v) v
  where
    readVar :: forall f a. EnvFunctor f => MultiVar a -> f (Result a)
    readVar (MultiVar a) = getCompose $ Free.runAp (Compose . readVar) (Free.Ap a)
