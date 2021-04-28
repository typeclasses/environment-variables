{-# language OverloadedStrings #-}

module Demo.Vars where

import Env

verbosity :: Var Integer
verbosity = Env.integerDecimal "VERBOSITY"

apiKey :: Name
apiKey = name "API_KEY"

apiSecret :: Name
apiSecret = name "API_SECRET"

home :: Name
home = name "HOME"
