module VarConversions where

import MultiVar (Multi (..))
import qualified MultiVar
import OneOptionalVar (Opt (Opt))
import OneRequiredVar (Var (Var))

import Data.Function ((.), id)
import Data.Functor (fmap)
import Data.Maybe (Maybe (..))

optional ::
    value -- ^ Value to return when the variable is absent in the environment.
    -> Var value -- ^ A required environment variable.
    -> Opt value -- ^ An optional environment variable.
optional d (Var x f) = Opt x d f

optionalMaybe ::
    Var value -- ^ A required environment variable.
    -> Opt (Maybe value) -- ^ An optional environment variable. Returns a 'Just' value when the variable is present in the environment. Returns a 'Nothing' value when the variable is absent in the environment.
optionalMaybe (Var x f) = Opt x Nothing (fmap Just . f)

class ToMulti v
  where
    multi :: v a -> Multi a

instance ToMulti Var
  where
    multi = ApVar (Pure id)

instance ToMulti Opt
  where
    multi = ApOpt (Pure id)
