{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module TestLib where

import           Data.Bifunctor
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.List
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Prana.Decode
import           Prana.Types
import           System.Exit
import           System.IO.Temp
import           System.Process

data CompileType = Normal | Bare

-- | Compile a single module.
compileModule :: CompileType -> String -> String -> IO [Bind]
compileModule ty name contents =
  fmap (snd . head) (compileModulesWith ty [(name, contents)])

-- | Compile the given sources as a set of modules.
compileModulesWith :: CompileType -> [(String,String)] -> IO [(String, [Bind])]
compileModulesWith ty modules =
  withSystemTempDirectory
    "prana-compile"
    (\dir -> do
       let fps = map (\(moduleName, src) -> (moduleName ++ ".hs", src)) modules
       mapM_ (\(fp, src) -> writeFile (dir ++ "/" ++ fp) src) fps
       (code, out, err) <- compileFile ty dir (map fst fps)
       case code of
         ExitSuccess ->
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
         ExitFailure {} -> error (unlines ["Compile failed:", out, err]))

-- | Run a compile with docker in the given dir on the given file.
compileFile :: CompileType -> FilePath -> [FilePath] -> IO (ExitCode, String, String)
compileFile ty pwd fps = do
  case ty of
    Normal ->
      readProcessWithExitCode
        "docker"
        ([ "run"
         , "-v" ++ pwd ++ ":" ++ pwd
         , "-w" ++ pwd
         , "--rm"
         , "ghc-compile"
         , "ghc"
         , "-O0"
         , "-fbyte-code"
         , "-this-unit-id"
         , "prana-test"
         ] ++
         fps)
        ""
    Bare ->
      readProcessWithExitCode
        "docker"
        ([ "run"
         , "-v" ++ pwd ++ ":" ++ pwd
         , "-w" ++ pwd
         , "--rm"
         , "ghc-compile"
         , "sh"
         , "-c"
         , intercalate
             ";"
             [ "rm /root/prana/names.txt"
             , "touch /root/prana/names.txt"
             , unwords
                 (["ghc", "-O0", "-fbyte-code", "-this-unit-id", "prana-test"] ++
                  fps)
             ]
         ])
        ""

-- | Drop the module header that's not of use to us in the test-suite.
dropModuleHeaders :: [(String, [Bind])] -> [(String, [Bind])]
dropModuleHeaders = map (second (filter (not . header)))
  where
    header (Bind { bindVar = ExportedIndex _
                 , bindExp = AppE (AppE (ConE ConId) (AppE (ConE ConId) (LitE (Str "prana-test")))) (AppE (ConE ConId) (LitE (Str _)))
                 }) = True
    header _ = False

link :: [(String, [Bind])] -> Vector Exp
link = V.fromList . concatMap (\(_, bs) -> map bindExp bs)
