{-# language FlexibleContexts, NoImplicitPrelude #-}

module Env.Ops where

import Env

import Control.Applicative ((<*>))
import Data.Semigroup ((<>))

(*) :: Lift (Product a) x => Product (a -> b) -> x -> Product b
p * x = p <*> lift x

(+) :: (Lift (Sum a) x, Lift (Sum a) y) => x -> y -> Sum a
x + y = lift x <> lift y
