{-# language NamedFieldPuns, OverloadedStrings #-}

module Demo.Environments where

import Env
import Env.Environment

data DemoEnv = DemoEnv{ demoEnvName :: Text, demoEnvDescription :: Text, demoEnvironment :: Environment }

base :: DemoEnv
base = DemoEnv{ demoEnvName, demoEnvDescription, demoEnvironment }
  where
    demoEnvName = "base"
    demoEnvDescription = "Base environment with some typical things"
    demoEnvironment =
      EnvironmentList
        [ Item "USER" "chris"
        , Item "HOME" "/home/chris"
        , Item "HOSTNAME" "cubby"
        , Item "PWD" "/home/chris/environment-variables"
        , Item "XDG_RUNTIME_DIR" "/user/run/1000"
        , Item "SHELL" "/run/current-system/sw/bin/bash"
        , Item "LANG" "en_US.UTF-8"
        , Item "PATH" "/run/wrappers/bin:/run/current-system/sw/bin"
        ]

app :: DemoEnv
app = DemoEnv{ demoEnvName, demoEnvDescription, demoEnvironment }
  where
    demoEnvName = "app"
    demoEnvDescription = "App environment consisting of the base environment plus some things added specifically for the application"
    demoEnvironment =
      b <>
      EnvironmentList
        [ Item "VERBOSITY" "2"
        , Item "API_KEY" "j91bD2ncr"
        , Item "API_SECRET" "i12e9vnd32"
        ]
    DemoEnv{ demoEnvironment = b } = base

problem :: DemoEnv
problem = DemoEnv{ demoEnvName, demoEnvDescription, demoEnvironment }
  where
    demoEnvName = "problem"
    demoEnvDescription = "Problematic environment wherein everything has gone wrong"
    demoEnvironment = EnvironmentList [Item "VERBOSITY" "abc"]
