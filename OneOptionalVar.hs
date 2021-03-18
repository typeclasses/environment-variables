module OneOptionalVar where

import Data.Functor
import Data.Maybe
import Data.Text (Text)

import Name

data OneOptionalVar a = OneOptionalVar Name a (Text -> Maybe a)
    deriving stock Functor

var :: Name -> a -> (Text -> Maybe a) -> OneOptionalVar a
var = OneOptionalVar
