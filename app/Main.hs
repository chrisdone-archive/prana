{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception
import           Data.Bifunctor
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Tuple
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
  print binds
  {-case find ((== "$main$Main$main") . idStableName . bindI) binds of
    Nothing -> error "No name found for main."
    Just main_uniq ->
      case M.lookup (Id main_uniq) binds of
        Nothing -> error "Couldn't lookup main from bindings."
        Just e ->
          catch
            (runInterpreter binds nameMap e >>= print)
            (\case
               NotInScope (Id id' _) ->
                 error ("Not in scope: " ++ show id' ++ " (" ++ show id' ++ ")")
               err -> error (show err))-}
  pure ()
