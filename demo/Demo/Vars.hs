{-# language NoImplicitPrelude, OverloadedStrings #-}

module Demo.Vars where

import Env
import Env.Ops
import Env.Sundries

import Prelude (Bool, Integer, Either (..), Maybe (..), fmap, pure)

verbosity :: Required Integer
verbosity = integerDecimal "VERBOSITY"

apiKey :: Name
apiKey = NameText "API_KEY"

apiSecret :: Name
apiSecret = NameText "API_SECRET"

apiCredentials :: Product (Text, Text)
apiCredentials = pure (,) * apiKey * apiSecret

home :: Name
home = NameText "HOME"

homeIsPresent :: Optional Bool
homeIsPresent = isPresent home

homeAndVerbosity :: Product (Text, Integer)
homeAndVerbosity = pure (,) * home * verbosity

homeOrVerbosity :: Sum (Either Text Integer)
homeOrVerbosity = fmap Left (parse home Just) + fmap Right verbosity

verbosityOrHome :: Sum (Either Text Integer)
verbosityOrHome = fmap Right verbosity + fmap Left (parse home Just)

user :: Name
user = NameText "USER"

homeOrUser :: Sum Text
homeOrUser = home + user

locale :: Sum Text
locale = NameText "LANG" + NameText "LC_ALL"
