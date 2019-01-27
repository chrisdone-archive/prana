{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test all things on the prana side from compiling, reading .prana
-- files, sanity checks, interpreting, etc.

module Main where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import           Prana.Decode
import           Prana.Types
import           System.Exit
import           System.IO.Temp
import           System.Process
import           Test.Hspec

-- | Main entry point.
main :: IO ()
main = hspec spec

-- | Test suite spec.
spec :: Spec
spec = describe "Compile and Decode" compileAndDecode

-- | Test compiling and decoding.
compileAndDecode :: Spec
compileAndDecode =
  it
    "Compile id"
    (do idmod <-
          compileModules
            [ ("Id", "module Id where id x = x")
            , ( "On"
              , "module On where\n\
                \on :: (b -> b -> c) -> (a -> b) -> a -> a -> c\n\
                \(.*.) `on` f = \\x y -> f x .*. f y")
            ]
        shouldBe
          idmod
          [ ( "Id"
            , [ Bind
                  { bindVar = ExportedIndex 6609
                  , bindExp = LamE (LocalIndex 57218) (VarE (LocalIndex 57218))
                  }
              ])
          , ( "On"
            , [ Bind
                  { bindVar = ExportedIndex 6611
                  , bindExp =
                      LamE
                        (LocalIndex 57222)
                        (LamE
                           (LocalIndex 57223)
                           (LamE
                              (LocalIndex 57224)
                              (LamE
                                 (LocalIndex 57225)
                                 (AppE
                                    (AppE
                                       (VarE (LocalIndex 57222))
                                       (AppE
                                          (VarE (LocalIndex 57223))
                                          (VarE (LocalIndex 57224))))
                                    (AppE
                                       (VarE (LocalIndex 57223))
                                       (VarE (LocalIndex 57225)))))))
                  }
              ])
          ])

-- | Compile a single module.
compileModule :: String -> String -> IO [Bind]
compileModule name contents =
  fmap (snd . head) (compileModules [(name, contents)])

-- | Compile the given sources as a set of modules.
compileModules :: [(String,String)] -> IO [(String, [Bind])]
compileModules modules =
  withSystemTempDirectory
    "prana-compile"
    (\dir -> do
       let fps = map (\(moduleName, src) -> (moduleName ++ ".hs", src)) modules
       mapM_ (\(fp, src) -> writeFile (dir ++ "/" ++ fp) src) fps
       (code, out, err) <- compileFile dir (map fst fps)
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
                  Right (_, _, binds) -> pure (moduleName, dropModuleHeader binds))
             modules
         ExitFailure {} -> error (unlines ["Compile failed:", out, err]))

-- | Run a compile with docker in the given dir on the given file.
compileFile :: FilePath -> [FilePath] -> IO (ExitCode, String, String)
compileFile pwd fps = do
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
     , "-v0"
     ] ++
     fps)
    ""

-- | Drop the module header that's not of use to us in the test-suite.
dropModuleHeader :: [Bind] -> [Bind]
dropModuleHeader = filter (not . header)
  where
    header (Bind { bindVar = ExportedIndex _
                 , bindExp = AppE (AppE (ConE ConId) (AppE (ConE ConId) (LitE (Str "prana-test")))) (AppE (ConE ConId) (LitE (Str _)))
                 }) = True
    header _ = False
