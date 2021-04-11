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
    UseNoVars :: a -> Product a
    UseOneVar :: Var a -> Product a
    UseOneOpt :: Opt a -> Product a
    UseManyVars :: Product (a -> b) -> Product a -> Product b

instance IsString (Product Text)
  where
    fromString = UseOneVar . fromString

instance IsString (Product (Maybe Text))
  where
    fromString = UseOneOpt . fromString

instance Functor Product
  where
    fmap f = \case
      UseNoVars x -> UseNoVars (f x)
      UseOneVar x -> UseOneVar (fmap f x)
      UseOneOpt x -> UseOneOpt (fmap f x)
      UseManyVars mf ma -> UseManyVars (fmap (f .) mf) ma

instance Applicative Product
  where
    pure = UseNoVars

    (<*>) :: forall a b. Product (a -> b) -> Product a -> Product b
    mf <*> (multi_a :: Product a) =
      case mf of
        UseNoVars (f :: a -> b) -> fmap f multi_a
        UseOneVar vf -> UseManyVars (UseOneVar vf) multi_a
        UseOneOpt vf -> UseManyVars (UseOneOpt vf) multi_a
        UseManyVars (multi_cab :: Product (c -> a -> b)) (v :: Product c) -> UseManyVars multi_cb v
          where
            multi_cb :: Product (c -> b)
            multi_cb = pure (\f c a -> f a c) <*> multi_cab <*> multi_a
