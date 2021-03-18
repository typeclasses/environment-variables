module Name where

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Hashable (Hashable)
import Data.Monoid (Monoid)
import Data.Ord (Ord)
import Data.Semigroup (Semigroup)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show (Show)

-- | The name of an environment variable.
newtype Name = NameText { nameText :: Text }
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass (Hashable)
    deriving newtype (IsString, Semigroup, Monoid)
