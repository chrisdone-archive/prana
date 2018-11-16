module Main where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import           Prana.Decode
import           Prana.Types
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  _modules <-
    mapM
      (\fp -> do
         bytes <- L.readFile fp
         case runGetOrFail (decodeArray decodeBind) bytes of
           Left e -> error ("failed to decode " ++ fp ++ ": " ++ show e ++ ", file contains: "++ take 10 (show bytes))
           Right (_,_,m) -> pure (m :: [Bind]))
      args
  pure ()
