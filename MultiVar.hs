module MultiVar where

import Control.Applicative (Applicative (..))
import Data.Function ((.))
import Data.Functor (Functor (..))
import Data.Maybe (Maybe (..))
import Data.String (IsString (fromString))
import Data.Text (Text)

import Var (Var, Opt)

-- | The product of multiplying any number of individual environment variables. Construct 'Product' values using 'lift', 'Applicative' combinators, and string overloading.
data Product a
  where
    Zero :: a -> Product a
    OneVar :: Var a -> Product a
    OneOpt :: Opt a -> Product a
    Many :: Product (a -> b) -> Product a -> Product b

instance IsString (Product Text)
  where
    fromString = OneVar . fromString

instance IsString (Product (Maybe Text))
  where
    fromString = OneOpt . fromString

instance Functor Product
  where
    fmap f = \case
      Zero x -> Zero (f x)
      OneVar x -> OneVar (fmap f x)
      OneOpt x -> OneOpt (fmap f x)
      Many mf ma -> Many (fmap (f .) mf) ma

instance Applicative Product
  where
    pure = Zero

    (<*>) :: forall a b. Product (a -> b) -> Product a -> Product b
    mf <*> (multi_a :: Product a) =
      case mf of
        Zero (f :: a -> b) -> fmap f multi_a
        OneVar vf -> Many (OneVar vf) multi_a
        OneOpt vf -> Many (OneOpt vf) multi_a
        Many (multi_cab :: Product (c -> a -> b)) (v :: Product c) -> Many multi_cb v
          where
            multi_cb :: Product (c -> b)
            multi_cb = pure (\f c a -> f a c) <*> multi_cab <*> multi_a
