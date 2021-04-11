module MultiVar where

import Control.Applicative (Applicative (..))
import Data.Function ((.))
import Data.Functor (Functor (..))

import Var (Var, Opt)

data Multi a
  where
    Zero :: a -> Multi a
    OneVar :: Var a -> Multi a
    OneOpt :: Opt a -> Multi a
    Many :: Multi (a -> b) -> Multi a -> Multi b

instance Functor Multi
  where
    fmap f = \case
      Zero x -> Zero (f x)
      OneVar x -> OneVar (fmap f x)
      OneOpt x -> OneOpt (fmap f x)
      Many mf ma -> Many (fmap (f .) mf) ma

instance Applicative Multi
  where
    pure = Zero

    (<*>) :: forall a b. Multi (a -> b) -> Multi a -> Multi b
    mf <*> (multi_a :: Multi a) =
      case mf of
        Zero (f :: a -> b) -> fmap f multi_a
        OneVar vf -> Many (OneVar vf) multi_a
        OneOpt vf -> Many (OneOpt vf) multi_a
        Many (multi_cab :: Multi (c -> a -> b)) (v :: Multi c) -> Many multi_cb v
          where
            multi_cb :: Multi (c -> b)
            multi_cb = pure (\f c a -> f a c) <*> multi_cab <*> multi_a
