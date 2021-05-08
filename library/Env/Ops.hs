{-# options_ghc -Wall #-}

{-# language FlexibleContexts, NoImplicitPrelude #-}

module Env.Ops where

import Env

import Control.Applicative ((<*>))
import Data.Semigroup ((<>))

(*) :: Lift (Product a) x => Product (a -> b) -> x -> Product b
p * x = p <*> lift x

(+) :: (Addend a x, Addend a y) => x -> y -> Sum a
x + y = addend x <> addend y
