module VarConversions where

import MultiVar (MultiVar)
import qualified MultiVar
import OneOptionalVar (Opt (Opt))
import OneRequiredVar (Var (Var))
import OneVar (OneVar)
import qualified OneVar

import Data.Function ((.))
import Data.Functor (fmap)
import Data.Maybe (Maybe (..))

required :: Opt a -> Var a
required (Opt x _ f) = Var x f

optional :: a -> Var a -> Opt a
optional d (Var x f) = Opt x d f

optionalMaybe :: Var a -> Opt (Maybe a)
optionalMaybe (Var x f) = Opt x Nothing (fmap Just . f)

class ToOneVar v
  where
    oneVar :: v a -> OneVar a

instance ToOneVar Var
  where
    oneVar (Var x f) = OneVar.Required x f

instance ToOneVar Opt
  where
    oneVar (Opt x d f) = OneVar.Optional x d f

class ToMulti v
  where
    multi :: v a -> MultiVar a

instance ToMulti OneVar
  where
    multi = MultiVar.multi

instance ToMulti Var
  where
    multi = multi . oneVar

instance ToMulti Opt
  where
    multi = multi . oneVar
