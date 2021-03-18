module Name where

import Data.Data
import Data.Eq
import Data.Hashable
import Data.Monoid
import Data.Ord
import Data.Semigroup
import Data.String
import Data.Text (Text)
import GHC.Generics
import Text.Show

-- | The name of an environment variable.
newtype Name = Name { nameText :: Text }
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass (Hashable)
    deriving newtype (IsString, Semigroup, Monoid)
