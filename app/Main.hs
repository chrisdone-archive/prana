{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import           Data.Generics
import qualified Data.Map.Strict as M
import           Prana.Decode
import           Prana.Interpret
import           Prana.Types
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  (indices, binds) <-
    fmap
      (\xs -> (concatMap fst xs, concatMap snd xs))
      (mapM
         (\fp -> do
            bytes <- L.readFile fp
            case runGetOrFail
                   ((,) <$> decodeArray decodeMethodIndex <*>
                    decodeArray decodeBind)
                   bytes of
              Left e ->
                error
                  ("failed to decode " ++
                   fp ++
                   ": " ++ show e ++ ", file contains: " ++ take 10 (show bytes))
              Right (_, _, (indices, binds)) -> do
                pure
                  ( indices
                  , everywhere
                      (mkT
                         (\case
                            AppE e TypE {} -> e
                            e -> e))
                      binds))
         args)
  let globals =
        M.fromList
          (concatMap
             (\case
                NonRec v e -> [(v, e)]
                Rec bs -> bs)
             binds)
      methods = M.fromList indices
  -- From: https://mail.haskell.org/pipermail/ghc-devs/2018-November/016592.html
  -- > I've also learned that GHC wraps the* Main.main* function with another
  -- > function called *:Main.main* which is the first function called by the RTS.
  case M.lookup "main:Main.main" (M.mapKeys idStableName globals) of
    Nothing -> error "Couldn't find main function."
    Just e ->
      catch
        (runInterpreter globals methods (e) >>= print) {-AppE e (LitE (Str "RealWorld"))-}
        (\case
           NotInScope i ->
             error $
             ("Not in scope: " ++ show i ++ "\n") ++
             ("Methods\n" ++
              unlines (map show (M.keys methods)) ++
              "\n" ++ "Scope\n" ++ unlines (map show (M.keys globals)))
           err -> error (show err))
