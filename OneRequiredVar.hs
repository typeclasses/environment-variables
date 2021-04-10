module OneRequiredVar where

import Data.Functor (Functor)
import Data.Maybe (Maybe)
import Data.Text (Text)

import Name (Name)

-- | A single required environment variable.
data Var value =
    Var
      Name -- ^ The name of the environment variable to read.
      (Text -> Maybe value) -- ^ How to parse the text into a value.
    deriving stock Functor

var :: Name -> (Text -> Maybe value) -> Var value
var = Var
