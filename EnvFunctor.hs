module EnvFunctor where

import Control.Applicative (Applicative)
import Data.Function ((.))
import Data.Functor (fmap)
import Data.Maybe (Maybe)
import System.IO (IO)

import Data.Text (Text)
import qualified Data.Text as Text

import EnvData (Environment)
import qualified EnvData as Environment
import Name (Name, nameText)

import qualified System.Environment as Sys

class Applicative context => Context context
  where
    lookup :: Name -> context (Maybe Text)

instance Context IO
  where
    lookup =
        fmap (fmap Text.pack)
        . Sys.lookupEnv
        . Text.unpack
        . nameText

instance Context ((->) Environment)
  where
    lookup = Environment.lookup
