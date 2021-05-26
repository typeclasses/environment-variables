{-# options_ghc -Wall #-}

{-# language NoImplicitPrelude #-}

import Tutorial

import System.IO (IO)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText

main :: IO ()
main =
  do
    LazyText.putStr tutorial
    LBS.writeFile "tutorial/output.txt" (LazyText.encodeUtf8 tutorial)
