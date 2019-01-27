{-# LANGUAGE TupleSections #-}
-- |

module Main where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           System.Directory
import           System.Exit
import           System.IO.Temp
import           System.Process
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()

-- | Compile the given sources as a set of modules.
compileModules :: [(String,String)] -> IO (Maybe [(String, ByteString)])
compileModules modules =
  withSystemTempDirectory
    "prana-compile"
    (\dir -> do
       let fps = map (\(moduleName, src) -> (moduleName ++ ".hs", src)) modules
       mapM_ (\(fp, src) -> writeFile (dir ++ "/" ++ fp) src) fps
       code <- compileFile dir (map fst fps)
       case code of
         ExitSuccess ->
           fmap
             sequence
             (mapM
                (\(moduleName, _) ->
                   fmap
                     (Just . (moduleName, ))
                     (S.readFile
                        (dir ++ "/prana-test_" ++ moduleName ++ ".prana")))
                modules)
         ExitFailure {} -> pure Nothing)

-- | Run a compile with docker in the given dir on the given file.
compileFile :: FilePath -> [FilePath] -> IO ExitCode
compileFile pwd fps = do
  rawSystem
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
