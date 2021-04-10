module VarConversions where

import MultiVar (MultiVar)
import qualified MultiVar
import OneOptionalVar (OneOptionalVar (OneOptionalVar))
import OneRequiredVar (Var (Var))
import OneVar (OneVar)
import qualified OneVar

import Data.Function ((.))
import Data.Functor (fmap)
import Data.Maybe (Maybe (..))

required :: OneOptionalVar a -> Var a
required (OneOptionalVar x _ f) = Var x f

optional :: a -> Var a -> OneOptionalVar a
optional d (Var x f) = OneOptionalVar x d f

optionalMaybe :: Var a -> OneOptionalVar (Maybe a)
optionalMaybe (Var x f) = OneOptionalVar x Nothing (fmap Just . f)

class ToOneVar v
  where
    oneVar :: v a -> OneVar a

instance ToOneVar Var
  where
    oneVar (Var x f) = OneVar.Required x f

instance ToOneVar OneOptionalVar
  where
    oneVar (OneOptionalVar x d f) = OneVar.Optional x d f

class ToMulti v
  where
    multi :: v a -> MultiVar a

instance ToMulti OneVar
  where
    multi = MultiVar.multi

instance ToMulti Var
  where
    multi = multi . oneVar

instance ToMulti OneOptionalVar
  where
    multi = multi . oneVar
