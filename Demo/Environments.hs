{-# language NamedFieldPuns, OverloadedStrings #-}

module Demo.Environments where

import Env

import Data.Text (Text)

data DemoEnv = DemoEnv{ demoEnvName :: Text, demoEnvDescription :: Text, demoEnvironment :: Environment }

base :: DemoEnv
base = DemoEnv{ demoEnvName, demoEnvDescription, demoEnvironment }
  where
    demoEnvName = "base"
    demoEnvDescription = "Base environment with some typical things"
    demoEnvironment =
      envs
        [ item "USER" "chris"
        , item "HOME" "/home/chris"
        , item "HOSTNAME" "cubby"
        , item "PWD" "/home/chris/environment-variables"
        , item "XDG_RUNTIME_DIR" "/user/run/1000"
        , item "SHELL" "/run/current-system/sw/bin/bash"
        , item "LANG" "en_US.UTF-8"
        , item "PATH" "/run/wrappers/bin:/run/current-system/sw/bin"
        ]

app :: DemoEnv
app = DemoEnv{ demoEnvName, demoEnvDescription, demoEnvironment }
  where
    demoEnvName = "app"
    demoEnvDescription = "App environment consisting of the base environment plus some things added specifically for the application"
    demoEnvironment =
      b <>
      envs
        [ item "VERBOSITY" "2"
        , item "API_KEY" "j91bD2ncr"
        , item "API_SECRET" "i12e9vnd32"
        ]
    DemoEnv{ demoEnvironment = b } = base

problem :: DemoEnv
problem = DemoEnv{ demoEnvName, demoEnvDescription, demoEnvironment }
  where
    demoEnvName = "problem"
    demoEnvDescription = "Problematic environment wherein everything has gone wrong"
    demoEnvironment = envs [item "VERBOSITY" "abc"]
