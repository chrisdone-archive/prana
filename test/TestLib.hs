{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module TestLib where

import           Data.Bifunctor
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Int
import           Data.List
import           Data.Maybe
import           Prana.Decode
import           Prana.Types
import           System.Exit
import           System.IO.Temp
import           System.Process

data CompileType = Normal

-- | Compile a single module.
compileModule :: CompileType -> String -> String -> IO ([Bind], [(MethodId, Int64)])
compileModule ty name contents =
  fmap (first (snd . head)) (compileModulesWith ty [(name, contents)])

-- | Compile the given sources as a set of modules.
compileModulesWith :: CompileType -> [(String,String)] -> IO ([(String, [Bind])], [(MethodId, Int64)])
compileModulesWith ty modules =
  withSystemTempDirectory
    "prana-compile"
    (\dir -> do
       let fps = map (\(moduleName, src) -> (moduleName ++ ".hs", src)) modules
       mapM_ (\(fp, src) -> writeFile (dir ++ "/" ++ fp) src) fps
       (code, out, err) <- compileFile ty dir (map fst fps)
       case code of
         ExitSuccess -> do
           mods <-
             mapM
               (\(moduleName, _) -> do
                  bytes <-
                    L.readFile (dir ++ "/prana-test_" ++ moduleName ++ ".prana")
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
                    Right (_, _, binds) -> pure (moduleName, binds))
               modules
           bytes <- L.readFile (dir <> "/names-cache.db")
           case runGetOrFail
                  (decodeArray decodeExportedId *> decodeArray decodeLocalId *>
                   decodeArray decodeConstrId *>
                   decodeArray decodeMethodId)
                  bytes of
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
             Right (_, _, methIds) -> pure (mods, methIds)
         ExitFailure {} -> error (unlines ["Compile failed:", out, err]))

-- | Run a compile with docker in the given dir on the given file.
compileFile :: CompileType -> FilePath -> [FilePath] -> IO (ExitCode, String, String)
compileFile ty pwd fps = do
  case ty of
    Normal ->
      (if False
         then readIt
         else readProcessWithExitCode)
        "docker"
        ([ "run"
         , "-v" ++ pwd ++ ":" ++ pwd
         , "-w" ++ pwd
         , "--rm"
         , "ghc-compile"
         , "sh"
         , "-c"
         , intercalate
             "&&"
             [unwords
                (["ghc", "-O0", "-fbyte-code", "-this-unit-id", "prana-test"] ++
                 fps)
             ,"cp /root/prana/names-cache.db " ++ pwd]
         ])
        ""
  where
    readIt n args _ = do
      code <- System.Process.rawSystem n args
      pure (code, "", "")

-- | Drop the module header that's not of use to us in the test-suite.
dropModuleHeaders :: [(String, [Bind])] -> [(String, [Bind])]
dropModuleHeaders = map (second (filter (not . header)))
  where
    header (Bind { bindVar = ExportedIndex _
                 , bindExp = AppE (AppE (ConE (ConId _)) (AppE (ConE (ConId _)) (LitE (Str "prana-test")))) (AppE (ConE (ConId _)) (LitE (Str _)))
                 }) = True
    header _ = False

link :: [(String, [Bind])] -> HashMap Int64 Exp
link mods = globals
  where
    bs = concatMap snd mods
    globals =
      HM.fromList
        (mapMaybe
           (\b ->
              case bindVar b of
                ExportedIndex i -> Just (i, bindExp b)
                _ -> Nothing)
           bs)
