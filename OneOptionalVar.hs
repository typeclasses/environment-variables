module OneOptionalVar where

import Data.Functor (Functor)
import Data.Maybe (Maybe)
import Data.Text (Text)

import Name (Name)

data OneOptionalVar a = OneOptionalVar Name a (Text -> Maybe a)
    deriving stock Functor

var :: Name -> a -> (Text -> Maybe a) -> OneOptionalVar a
var = OneOptionalVar
