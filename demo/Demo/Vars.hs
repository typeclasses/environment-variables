{-# language NoImplicitPrelude, OverloadedStrings #-}

module Demo.Vars where

import Env
import Env.Ops

import Prelude (Integer, pure)

verbosity :: Var Integer
verbosity = Env.integerDecimal "VERBOSITY"

apiKey :: Name
apiKey = name "API_KEY"

apiSecret :: Name
apiSecret = name "API_SECRET"

apiCredentials :: Product (Text, Text)
apiCredentials = pure (,) * apiKey * apiSecret

home :: Name
home = name "HOME"

homeAndVerbosity :: Product (Text, Integer)
homeAndVerbosity = pure (,) * home * verbosity

user :: Name
user = name "USER"

homeOrUser :: Sum Text
homeOrUser = home + user
