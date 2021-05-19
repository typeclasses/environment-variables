{-# language NoImplicitPrelude, OverloadedStrings #-}

module Demo.Vars where

import Env
import Env.Ops
import Env.Sundries

import Data.Function ((&))
import Prelude (Bool, Integer, Either (..), Maybe (..), fmap, pure)

verbosity :: Required Integer
verbosity = "VERBOSITY" & parse integerDecimal

verbosityWithDefault :: Optional Integer
verbosityWithDefault = verbosity & optional 1

apiKey :: Name
apiKey = NameText "API_KEY"

apiSecret :: Name
apiSecret = NameText "API_SECRET"

apiCredentials :: Product (Text, Text)
apiCredentials = pure (,) * apiKey * apiSecret

userAndApiCredentials :: Product (Text, Text, Text)
userAndApiCredentials = pure (,,) * user * apiKey * apiSecret

home :: Name
home = NameText "HOME"

homeIsPresent :: Optional Bool
homeIsPresent = isPresent home

homeAndVerbosity :: Product (Text, Integer)
homeAndVerbosity = pure (,) * home * verbosity

homeOrVerbosity :: Sum (Either Text Integer)
homeOrVerbosity = fmap Left (text home) + fmap Right verbosity

verbosityOrHome :: Sum (Either Text Integer)
verbosityOrHome = fmap Right verbosity + fmap Left (text home)

user :: Name
user = NameText "USER"

homeOrUser :: Sum Text
homeOrUser = home + user

locale :: Sum Text
locale = NameText "LANG" + NameText "LC_ALL"
