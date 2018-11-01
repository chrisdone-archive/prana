-- |

module Prana.Debug where

import           Control.Monad.IO.Class
import           Data.Binary.Builder
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List
import           HIndent
import           HIndent.Types
import           Prana.Print

debugBinds binds =
  liftIO
    (L.putStrLn
       (either
          L8.pack
          toLazyByteString
          (reformat
             defaultConfig
             Nothing
             (S8.pack
                ("[" ++ intercalate ", " (map (showBind False) binds) ++ "]")))))
