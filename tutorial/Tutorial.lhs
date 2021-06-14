> {-# options_ghc -Wall -fno-warn-unused-top-binds #-}
> {-# language OverloadedStrings #-}
>
> module Tutorial (tutorial) where
>
> import Env
> import Env.Environment
>
> import qualified Data.Foldable as Foldable
> import qualified Data.List as List
> import qualified Data.Text.Lazy as LT
> import qualified Data.Text.Lazy.Builder as TB

In a typical shell environment, you likely have an environment variable named ‘USER’ that tells you the name of the account that is currently logged in.

> user :: Env.Name
> user = "USER"

To create an ‘IO’ action that obtains the value of the ‘USER’ variable from a process's environment, you use the ‘read’ function.

> getUserEither :: IO (Either Missing Text)
> getUserEither = Env.read user

Notice that the result type above is ‘Either Missing Text’. If all goes well, we get a ‘Text’ result. But if, for some reason, the environment does not actually contain a variable named ‘USER’, then action produces a ‘Missing’ value instead to indicate that this environment variable is missing.

If you don't care to deal with the situation in which the variable is missing, you can use the ‘readOrFail’ function, which throws an exception instead of returning an error value.

> getUserOrFail :: IO Text
> getUserOrFail = Env.readOrFail user

For the demonstrations in this tutorial, we will read from mock environments instead of working with ‘IO’. The same polymorphic ‘read’ function we used above also works in this situation.

> userEither1 :: Either Missing Text
> userEither1 = Env.read user $ EnvironmentList [ Item "USER" "chris" ]

This expression evaluates to `Right "Chris"`. If the environment variable were *not* present — such as in the following example where we attempt to read the same variable from an empty environment — we get a `Left` result instead.

> userEither2 :: Either Missing Text
> userEither2 = Env.read user $ EnvironmentList []

> user1 :: Text
> user1 = Env.test user $ EnvironmentList [ Item "USER" "chris" ]

> tutorial :: LT.Text
> tutorial =
>   TB.toLazyText $ Foldable.fold $
>     List.intersperse "\n\n" $
>       [ either errorMessageBuilder TB.fromText userEither1
>       , either errorMessageBuilder TB.fromText userEither2
>       , TB.fromText user1
>       ]
