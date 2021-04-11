module VarConversions where

import MultiVar (Product (..))
import qualified MultiVar
import Var (Var (Var), Opt (Opt))
import Name

import Data.Function ((.), id)
import Data.Functor (fmap)
import Data.Maybe (Maybe (..))
import Data.Text (Text)

optional ::
    value -- ^ Value to return when the variable is absent in the environment.
    -> Var value -- ^ A required environment variable.
    -> Opt value -- ^ An optional environment variable.
optional d (Var x f) = Opt x d f

optionalMaybe ::
    Var value -- ^ A required environment variable.
    -> Opt (Maybe value) -- ^ An optional environment variable. Returns a 'Just' value when the variable is present in the environment. Returns a 'Nothing' value when the variable is absent in the environment.
optionalMaybe (Var x f) = Opt x Nothing (fmap Just . f)
