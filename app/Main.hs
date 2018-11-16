module Main where

import qualified Data.ByteString as S
import qualified Persist
import           Prana.Types
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  _modules <-
    mapM
      (\fp -> do
         bytes <- S.readFile fp
         case Persist.decode bytes of
           Left e -> error ("failed to decode " ++ fp ++ ": " ++ e)
           Right m -> pure (m :: [Bind]))
      args
  pure ()
