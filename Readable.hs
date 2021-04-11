module Readable (Readable (readVar)) where

import Control.Applicative (Applicative (..))
import Data.Function ((.), ($))
import Data.Functor (fmap)
import Data.Functor.Compose (Compose (Compose), getCompose)
import Data.Maybe (Maybe, maybe)
import Data.Validation (Validation (Success, Failure), bindValidation)
import System.IO (IO)
import Data.Map (Map)

import Data.Text (Text)
import qualified Data.Text as Text

import EnvData (Environment)
import EnvFunctor (EnvFunctor, lookupEnv)
import MultiVar (Multi (..))
import Name (Name)
import OneOptionalVar (Opt (..))
import OneRequiredVar (Var (..))
import Problems (EnvFailure, Problem (..), oneProblemFailure)

{- |

Type parameters:

  - @var@ - The type of variable you want to read: 'Name', 'Var', 'Opt', or 'Multi'.
  - @value@ - What type of value is produced when an environment variable is successfully read.
  - @context@ - Normally 'IO', but possibly @('EnvData' ->)@ if you are reading from a mock environment.

-}
class Readable var value | var -> value
  where
    readVar :: EnvFunctor context => var -> context (Validation EnvFailure value)

justOr :: Problem -> Name -> Maybe a -> Validation EnvFailure a
justOr x name = maybe (Failure (oneProblemFailure x name)) Success

justOrMissing :: Name -> Maybe a -> Validation EnvFailure a
justOrMissing = justOr VarMissing

justOrInvalid :: Name -> Maybe a -> Validation EnvFailure a
justOrInvalid = justOr VarInvalid

instance Readable Name Text
  where
    readVar name = fmap (justOrMissing name) $ lookupEnv name

instance Readable (Var a) a
  where
    readVar (Var name parse) =
        fmap (`bindValidation` (justOrInvalid name . parse)) $
            readVar name

instance Readable (Opt a) a
  where
    readVar (Opt name def parse) =
        fmap (maybe (Success def) (justOrInvalid name . parse)) $
            lookupEnv name

instance Readable (Multi v) v
  where
    readVar :: forall f a. EnvFunctor f => Multi a -> f (Validation EnvFailure a)
    readVar = \case
      Pure x -> pure (Success x)
      ApVar mf v -> pure (<*>) <*> readVar mf <*> readVar v
      ApOpt mf v -> pure (<*>) <*> readVar mf <*> readVar v
