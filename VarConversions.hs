module VarConversions where

import MultiVar (Multi (..))
import qualified MultiVar
import OneOptionalVar (Opt (Opt))
import OneRequiredVar (Var (Var))

import Data.Function ((.), id)
import Data.Functor (fmap)
import Data.Maybe (Maybe (..))

optional :: a -> Var a -> Opt a
optional d (Var x f) = Opt x d f

optionalMaybe :: Var a -> Opt (Maybe a)
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
