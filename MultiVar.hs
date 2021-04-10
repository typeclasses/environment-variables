module MultiVar where

import Control.Applicative (Applicative (..))
import Data.Function ((.))
import Data.Functor (Functor (..))

import OneRequiredVar (Var)
import OneOptionalVar (Opt)

data Multi a
  where
    Pure :: a -> Multi a
    ApVar :: Multi (a -> b) -> Var a -> Multi b
    ApOpt :: Multi (a -> b) -> Opt a -> Multi b

instance Functor Multi
  where
    fmap f = \case
      Pure x -> Pure (f x)
      ApVar mf va -> ApVar (fmap (f .) mf) va
      ApOpt mf va -> ApOpt (fmap (f .) mf) va

instance Applicative Multi
  where
    pure = Pure

    (<*>) :: forall a b. Multi (a -> b) -> Multi a -> Multi b
    mf <*> (multi_a :: Multi a) =
      case mf of
        Pure (f :: a -> b) -> fmap f multi_a

        ApVar (multi_cab :: Multi (c -> a -> b)) (v :: Var c) -> ApVar multi_cb v
          where
            multi_cb :: Multi (c -> b)
            multi_cb = pure (\f c a -> f a c) <*> multi_cab <*> multi_a

        ApOpt (multi_cab :: Multi (c -> a -> b)) (v :: Opt c) -> ApOpt multi_cb v
          where
            multi_cb :: Multi (c -> b)
            multi_cb = pure (\f c a -> f a c) <*> multi_cab <*> multi_a
