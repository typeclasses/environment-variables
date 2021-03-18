module OneVar (OneVar (Required, Optional)) where

import Data.Functor (Functor)
import Data.Maybe (Maybe)
import Data.Text (Text)

import Name (Name)

data OneVar a
    = Required Name (Text -> Maybe a)
    | Optional Name a (Text -> Maybe a)
    deriving stock (Functor)
