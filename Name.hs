module Name where

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Hashable (Hashable)
import Data.Monoid (Monoid)
import Data.Ord (Ord)
import Data.Semigroup (Semigroup)
import Data.String (IsString, String)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show (Show)
import qualified Data.Text as Text

-- | The name of an environment variable.
newtype Name = NameText { nameText :: Text }
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass (Hashable)
    deriving newtype (IsString, Semigroup, Monoid)

pattern NameString :: String -> Name
pattern NameString s <- ((\(NameText t) -> Text.unpack t) -> s)
  where
    NameString s = NameText (Text.pack s)
