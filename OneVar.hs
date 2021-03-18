module OneVar where

import Data.Functor
import Data.Maybe
import Data.Text (Text)

import Name

data OneVar a
    = Required Name (Text -> Maybe a)
    | Optional Name a (Text -> Maybe a)
    deriving stock (Functor)

var :: Name -> (Text -> Maybe a) -> OneVar a
var = Required
