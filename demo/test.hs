{-# language NoImplicitPrelude #-}

import Demo.Output

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (when, (=<<))
import Data.Eq (Eq, (/=))
import System.Exit (exitFailure)
import System.IO (IO)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy.Encoding as LazyText

main :: IO ()
main = test (LazyText.encodeUtf8 demoOutput) =<< LBS.readFile "demo/demo.txt"

test :: Eq a => a -> a -> IO ()
test x y = when (x /= y) exitFailure
