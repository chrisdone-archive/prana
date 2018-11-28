{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import           Prana.Decode
import           Prana.Interpret
import           Prana.Types
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  binds <-
    fmap
      concat
      (mapM
         (\fp -> do
            bytes <- L.readFile fp
            case runGetOrFail (decodeArray decodeBind) bytes of
              Left e ->
                error
                  ("failed to decode " ++
                   fp ++
                   ": " ++ show e ++ ", file contains: " ++ take 10 (show bytes))
              Right (_, _, binds) -> do
                pure binds)
         args)
  let globals =
        M.fromList
          (concatMap
             (\case
                NonRec v e -> [(v, e)]
                Rec bs -> bs)
             binds)
      methods = mempty
  error
    ("Methods\n" ++
     unlines (map show (M.toList methods)) ++
     "\n" ++ "Scope\n" ++ unlines (map show (M.toList globals)))
  case M.lookup "main:Main:main" (M.mapKeys idStableName globals) of
    Nothing -> error "Couldn't find main function."
    Just e ->
      catch
        (runInterpreter globals methods e >>= print)
        (\case
           NotInScope (Id id' u) ->
             error
               ("Not in scope: " ++
                show id' ++
                " (" ++
                show u ++
                ")\n" ++ unlines (map (show . idStableName) (M.keys globals)))
           err -> error (show err))
