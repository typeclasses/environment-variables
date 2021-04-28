{-# options_ghc -Wall #-}

{-# language NoImplicitPrelude #-}

import Demo.Output

import Control.Applicative (Applicative (..))
import System.IO (IO)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText

main :: IO ()
main = LazyText.putStr demoOutput *> LBS.writeFile "demo/demo.txt" (LazyText.encodeUtf8 demoOutput)
