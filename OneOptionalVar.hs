module OneOptionalVar where

import Data.Functor (Functor)
import Data.Maybe (Maybe)
import Data.Text (Text)

import Name (Name)

-- | A single optional environment variable.
data Opt value =
    Opt
      Name -- ^ The name of the environment variable to read.
      value -- ^ A value to use instead of applying the parser if the name is not present in the environment.
      (Text -> Maybe value) -- ^ How to parse the text into a value.
    deriving stock Functor

var :: Name -> value -> (Text -> Maybe value) -> Opt value
var = Opt
