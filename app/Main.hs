{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception
import           Data.Bifunctor
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as L
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
  modules <-
    mapM
      (\fp -> do
         bytes <- L.readFile fp
         case runGetOrFail
                ((,) <$> decodeNameMap <*> decodeArray decodeBind)
                bytes of
           Left e ->
             error
               ("failed to decode " ++
                fp ++
                ": " ++ show e ++ ", file contains: " ++ take 10 (show bytes))
           Right (_, _, (nameMap, binds)) -> do
             pure (nameMap, binds))
      args
  let nameMap = mconcat (map fst modules)
      binds =
        M.fromList
          (concatMap
             (\case
                NonRec (Var v) e -> [(Id v, e)]
                Rec vs -> map (first (\(Var v) -> Id v)) vs)
             (concatMap snd modules))
  case lookup "$main$Main$main" (map swap (M.toList nameMap)) of
    Nothing -> error "No name found for main."
    Just main_uniq ->
      case M.lookup (Id main_uniq) binds of
        Nothing -> error "Couldn't lookup main from bindings."
        Just e ->
          catch
            (runInterpreter binds nameMap e >>= print)
            (\case
               NotInScope (Id id') ->
                 error
                   ("Not in scope: " ++
                    fromMaybe (show id') (fmap show (M.lookup id' nameMap)) ++
                    " (" ++ show id' ++ ")")
               err -> error (show err))
  pure ()
