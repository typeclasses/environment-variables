module Var where

import Data.Functor (Functor)
import Data.Maybe (Maybe)
import Data.Text (Text)

import Name (Name)

-- | A single required environment variable.
data Var value = Var Name (Text -> Maybe value)
    deriving stock Functor

var ::
    Name -- ^ The name of the environment variable to read.
    -> (Text -> Maybe value) -- ^ How to parse the text into a value.
    -> Var value
var = Var
