{-# options_ghc -Wall #-}
{-# language ExplicitNamespaces, PatternSynonyms #-}

module Env.Types
  (
    type Name, pattern NameText, pattern NameString,
    type Parser,
    type Default,
    type NameWithDefault,
    type Required,
    type Optional,
    type Var,
    type Composite,
    type NontrivialProduct,
    type Product,
    type Choice,
    type NontrivialSum,
    type Sum
  )
  where

import Env.Types'
