{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test all things on the prana side from compiling, reading .prana
-- files, sanity checks, interpreting, etc.

module Main where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Prana.Decode
import           Prana.Types
import           System.Exit
import           System.IO.Temp
import           System.Process
import           Test.Hspec

data CompileType = Normal | Bare

-- | Main entry point.
main :: IO ()
main = hspec spec

-- | Test suite spec.
spec :: Spec
spec = do
  describe "Compile and Decode" compileAndDecode
  describe "Dependencies" dependencies
  describe "Evaluation" evaluation

-- | Test evaluation of expressions.
evaluation :: Spec
evaluation = describe "Lambdas with local envs"
                      localLambdas

-- | Lambda evaluation with local evaluation.
localLambdas :: Spec
localLambdas =
  it
    "Id"
    (do idmod <-
          compileModulesWith
            Bare
            [ ("Id", "module Id where id x = x")
            , ( "On"
              , "module On where\n\
                              \import Id\n\
                              \const x _ = Id.id x")
            ]
        shouldBe
          idmod
          [ ( "Id"
            , [ Bind
                  { bindVar = ExportedIndex 0
                  , bindExp = LamE (LocalIndex 1) (VarE (LocalIndex 1))
                  }
              ])
          , ( "On"
            , [ Bind
                  { bindVar = ExportedIndex 2
                  , bindExp =
                      LamE
                        (LocalIndex 4)
                        (LamE
                           (LocalIndex 5)
                           (AppE (VarE (ExportedIndex 0)) (VarE (LocalIndex 4))))
                  }
              ])
          ])

-- | Test compiling and decoding.
dependencies :: Spec
dependencies =
  it
    "Compile two modules with interdependencies"
    (do idmod <-
          compileModulesWith Normal
            [ ("Id", "module Id where id x = x")
            , ( "On"
              , "module On where\n\
                \import Id\n\
                \const x _ = Id.id x")
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
                        (LocalIndex 57221)
                        (LamE
                           (LocalIndex 57222)
                           (AppE
                              (VarE (ExportedIndex 6609))
                              (VarE (LocalIndex 57221))))
                  }
              ])
          ])

-- | Test compiling and decoding.
compileAndDecode :: Spec
compileAndDecode =
  it
    "Compile id"
    (do idmod <-
          compileModulesWith Normal
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
                  Right (_, _, binds) -> pure (moduleName, dropModuleHeader binds))
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
         , "-v0"
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
                 ([ "ghc"
                  , "-O0"
                  , "-fbyte-code"
                  , "-this-unit-id"
                  , "prana-test"
                   -- , "-v0"
                  ] ++
                  fps)
             ]
         ])
        ""

-- | Drop the module header that's not of use to us in the test-suite.
dropModuleHeader :: [Bind] -> [Bind]
dropModuleHeader = filter (not . header)
  where
    header (Bind { bindVar = ExportedIndex _
                 , bindExp = AppE (AppE (ConE ConId) (AppE (ConE ConId) (LitE (Str "prana-test")))) (AppE (ConE ConId) (LitE (Str _)))
                 }) = True
    header _ = False
