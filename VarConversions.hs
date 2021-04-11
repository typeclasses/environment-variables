module VarConversions where

import MultiVar (Product (..))
import qualified MultiVar
import Var (Var (Var), Opt (Opt))
import Name

import Data.Function ((.), id)
import Data.Functor (fmap)
import Data.Maybe (Maybe (..))
import Data.Text (Text)

optional ::
    value -- ^ Value to return when the variable is absent in the environment.
    -> Var value -- ^ A required environment variable.
    -> Opt value -- ^ An optional environment variable.
optional d (Var x f) = Opt x d f

optionalMaybe ::
    Var value -- ^ A required environment variable.
    -> Opt (Maybe value) -- ^ An optional environment variable. Returns a 'Just' value when the variable is present in the environment. Returns a 'Nothing' value when the variable is absent in the environment.
optionalMaybe (Var x f) = Opt x Nothing (fmap Just . f)

-- | A /lift/ is a trivial function that converts from a smaller type to a more complex one. Since 'lift' is polymorphic in both domain and codomain, explicit type annotations are recommended.
class Lift b a where
    lift :: a -> b

instance Lift (Product a) (Var a) where
    lift = OneVar

instance Lift (Product a) (Opt a) where
    lift = OneOpt

instance Lift (Var Text) Name where
    lift x = Var x Just

instance Lift (Opt (Maybe Text)) Name where
    lift x = Opt x Nothing (Just . Just)

instance Lift (Product Text) Name where
    lift = lift . lift @(Var Text)

instance Lift (Product (Maybe Text)) Name where
    lift = lift . lift @(Opt (Maybe Text))
