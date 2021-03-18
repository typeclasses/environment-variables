module OneRequiredVar where

import Data.Functor
import Data.Maybe
import Data.Text (Text)

import Name

data OneRequiredVar a = OneRequiredVar Name (Text -> Maybe a)
    deriving stock Functor

var :: Name -> (Text -> Maybe a) -> OneRequiredVar a
var = OneRequiredVar
