{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Binary.Get
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as L
import           Data.Function
import           Data.List
import           Options.Applicative.Simple
import           Prana.Decode
import           Prana.Types

main :: IO ()
main = do
  (_, runCmd) <-
    simpleOptions "0" "Prana" "Prana desc" (pure ()) $ do
      addCommand
        "exported-names"
        "List all exported names"
        listExporteds
        (strOption (long "path"))
      addCommand
        "local-names"
        "List all local names"
        listLocals
        (strOption (long "path"))
      addCommand
        "dump-module"
        "Parse and dump a module"
        dumpModule
        (strOption (long "path"))
  runCmd

dumpModule :: FilePath -> IO ()
dumpModule path = do
  bytes <- L.readFile path
  case runGetOrFail (decodeArray decodeBind) bytes of
    Left (_, pos, e) ->
      error
        ("Decoding error! " ++
         e ++
         " at index " ++
         show pos ++
         "\n\nFile:\n\n" ++
         show (L.unpack bytes) ++
         "\n\nAt index:\n\n" ++
         show (drop (fromIntegral pos - 1) (L.unpack bytes)))
    Right (_, _, binds) -> mapM_ print binds

listExporteds :: FilePath -> IO ()
listExporteds path = do
  bytes <- L.readFile path
  case runGetOrFail (decodeArray decodeExportedId) bytes of
    Left e -> error (show e)
    Right (_, _, es) ->
      mapM_
        (\modules@((i0:_):_) -> do
           S8.putStrLn (exportedIdPackage i0)
           mapM_
             (\ids@(i:_) -> do
                S8.putStrLn ("\n  " <> exportedIdModule i <> "\n")
                mapM_ (S8.putStrLn . ("    " <>) . exportedIdName) ids)
             modules
           S8.putStrLn "")
        packages
      where packages =
              map
                (groupBy (on (==) exportedIdModule))
                (groupBy (on (==) exportedIdPackage) es)

listLocals :: FilePath -> IO ()
listLocals path = do
  bytes <- L.readFile path
  case runGetOrFail
         (decodeArray decodeExportedId *> decodeArray decodeLocalId)
         bytes of
    Left e -> error (show e)
    Right (_, _, es) ->
      mapM_
        (\modules@((i0:_):_) -> do
           S8.putStrLn (localIdPackage i0)
           mapM_
             (\ids@(i:_) -> do
                S8.putStrLn ("\n  " <> localIdModule i <> "\n")
                mapM_
                  (\n ->
                     S8.putStrLn
                       ("    " <> localIdName n <> " " <>
                        L.toStrict
                          (L.toLazyByteString
                             (L.int64HexFixed (unUnique (localIdUnique n))))))
                  ids)
             modules
           S8.putStrLn "")
        packages
      where packages =
              map
                (groupBy (on (==) localIdModule))
                (groupBy (on (==) localIdPackage) es)

{-main = do
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
           err -> error (show err))-}
