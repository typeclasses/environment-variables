module VarConversions where

import MultiVar (MultiVar)
import qualified MultiVar
import OneOptionalVar (OneOptionalVar (OneOptionalVar))
import OneRequiredVar (OneRequiredVar (OneRequiredVar))
import OneVar (OneVar)
import qualified OneVar

import Data.Function
import Data.Functor
import Data.Maybe

required :: OneOptionalVar a -> OneRequiredVar a
required (OneOptionalVar x _ f) = OneRequiredVar x f

optional :: a -> OneRequiredVar a -> OneOptionalVar a
optional d (OneRequiredVar x f) = OneOptionalVar x d f

optionalMaybe :: OneRequiredVar a -> OneOptionalVar (Maybe a)
optionalMaybe (OneRequiredVar x f) = OneOptionalVar x Nothing (fmap Just . f)

class ToOneVar v
  where
    oneVar :: v a -> OneVar a

instance ToOneVar OneRequiredVar
  where
    oneVar (OneRequiredVar x f) = OneVar.Required x f

instance ToOneVar OneOptionalVar
  where
    oneVar (OneOptionalVar x d f) = OneVar.Optional x d f

class ToMulti v
  where
    multi :: v a -> MultiVar a

instance ToMulti OneVar
  where
    multi = MultiVar.multi

instance ToMulti OneRequiredVar
  where
    multi = multi . oneVar

instance ToMulti OneOptionalVar
  where
    multi = multi . oneVar
