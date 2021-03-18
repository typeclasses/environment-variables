module OneRequiredVar where

import Data.Functor (Functor)
import Data.Maybe (Maybe)
import Data.Text (Text)

import Name (Name)

data OneRequiredVar a = OneRequiredVar Name (Text -> Maybe a)
    deriving stock Functor

var :: Name -> (Text -> Maybe a) -> OneRequiredVar a
var = OneRequiredVar
