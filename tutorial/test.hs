{-# language NoImplicitPrelude #-}

import Tutorial

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (when, (=<<))
import Data.Eq (Eq, (/=))
import System.Exit (exitFailure)
import System.IO (IO)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy.Encoding as LazyText

main :: IO ()
main =
  do
    x <- LBS.readFile "tutorial/output.txt"
    when (x /= LazyText.encodeUtf8 tutorial) exitFailure
