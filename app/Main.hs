{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import           Prana.Decode
import           Prana.Interpret
import           Prana.Types
import           System.Environment

main :: IO ()
main = do
  let fst3 (x,_,_) = x
      snd3 (_,x,_) = x
      thd3 (_,_,x) = x
  args <- getArgs
  (indices, enums, binds) <-
    fmap
      (\xs -> (concatMap fst3 xs, concatMap snd3 xs, concatMap thd3 xs))
      (mapM
         (\fp -> do
            bytes <- L.readFile fp
            case runGetOrFail
                   ((,,) <$> decodeArray decodeMethodIndex <*>
                    decodeArray decodeEnums <*>
                    decodeArray decodeBind)
                   bytes of
              Left e ->
                error
                  ("failed to decode " ++
                   fp ++
                   ": " ++ show e ++ ", file contains: " ++ take 10 (show bytes))
              Right (_, _, (indices, enums, binds)) -> do
                pure (indices, enums, binds))
         args)
  let !globals =
        (M.mapKeys
           idStableName
           (M.fromList
              (concatMap
                 (\case
                    NonRec v e -> [(v, e)]
                    Rec bs -> bs)
                 binds)))
      !methods = (M.mapKeys idStableName methods')
      methods' = M.fromList indices
      enums' = M.mapKeys idStableName (M.fromList enums)
  -- From: https://mail.haskell.org/pipermail/ghc-devs/2018-November/016592.html
  -- > I've also learned that GHC wraps the* Main.main* function with another
  -- > function called *:Main.main* which is the first function called by the RTS.
  case M.lookup "main:Demo.demo" globals
        of
    Nothing ->
      error $
      ("Can't find main!\n") ++
      ("Methods\n" ++
       unlines (map show (M.keys methods)) ++
       "\n" ++ "Scope\n" ++ unlines (map show (M.keys globals)))
    Just e -> do
      catch
        (do replicateM_ 1 (runInterpreter globals methods enums' e >>= print)
            pure ())
        (\case
           NotInScope i ->
             error $
             ("Not in scope: " ++ show i ++ "\n") ++
             ("Methods\n" ++
              unlines (map show (M.keys methods)) ++
              "\n" ++ "Scope\n" ++ unlines (map show (M.keys globals)))
           err -> error (show err))
