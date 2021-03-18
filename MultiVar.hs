module MultiVar where

import Control.Applicative
import Data.Coerce
import Data.Function
import Data.Functor

import qualified Control.Applicative.Free.Final as Free

import OneVar

newtype MultiVar a = MultiVar (forall g. Applicative g => (forall x. OneVar x -> g x) -> g a)
    deriving (Functor, Applicative) via (Free.Ap OneVar)

multi :: OneVar a -> MultiVar a
multi = coerce . Free.liftAp
