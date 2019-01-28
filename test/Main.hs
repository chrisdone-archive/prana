{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test all things on the prana side from compiling, reading .prana
-- files, sanity checks, interpreting, etc.

module Main where

import           Prana.Interpret
import           Prana.Types
import           Test.Hspec
import           TestLib

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
evaluation = do
  describe
    "Literals"
    (it
       "Ints"
       (do result <- eval mempty mempty (LitE (Int 123))
           shouldBe result (LitW (Int 123))))
  describe "Lambdas with local envs" localLambdas

-- | Lambda evaluation with local evaluation.
localLambdas :: Spec
localLambdas = do
  it
    "Id"
    (do idmod <-
          compileModulesWith
            Bare
            [ ("Id", "module Id where id x = x")
            , ( "On"
              , "module On where\n\
                \import Id\n\
                \const x _ = Id.id x\n\
                \it = On.const (123 :: Int) (57 :: Int)")
            ]
        let globals = link idmod
        result <- eval globals mempty (VarE (ExportedIndex 3))
        shouldBe
          result
          (ConW
             ConId
             [ Thunk
                 [ (1, VarE (LocalIndex 4))
                 , (4, AppE (ConE ConId) (LitE (Int 123)))
                 , (5, AppE (ConE ConId) (LitE (Int 57)))
                 ]
                 (LitE (Int 123))
             ]))
  it
    "Lets"
    (do idmod <-
          compileModulesWith
            Bare
            [ ( "Let"
              , "module Let where\n\
                \idem f x = let v = f x; g _ = (666::Int) in g (f v)\n\
                \it = idem (\\x -> x) (123 :: Int)")
            ]
        let globals = link idmod
        result <- eval globals mempty (VarE (ExportedIndex 1))
        shouldBe
          result
          (ConW
             ConId
             [ Thunk
                 [ (1, LamE (LocalIndex 5) (VarE (LocalIndex 5)))
                 , (2, AppE (ConE ConId) (LitE (Int 123)))
                 , ( 4
                   , AppE
                       (VarE (LocalIndex 1))
                       (AppE (VarE (LocalIndex 1)) (VarE (LocalIndex 2))))
                 ]
                 (LitE (Int 666))
             ]))

-- | Test compiling and decoding.
dependencies :: Spec
dependencies =
  it
    "Compile two modules with interdependencies"
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
          (dropModuleHeaders idmod)
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
compileAndDecode :: Spec
compileAndDecode =
  it
    "Compile id"
    (do idmod <-
          compileModulesWith
            Bare
            [ ("Id", "module Id where id x = x")
            , ( "On"
              , "module On where\n\
                \on :: (b -> b -> c) -> (a -> b) -> a -> a -> c\n\
                \(.*.) `on` f = \\x y -> f x .*. f y")
            ]
        shouldBe
          (dropModuleHeaders idmod)
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
                        (LocalIndex 5)
                        (LamE
                           (LocalIndex 6)
                           (LamE
                              (LocalIndex 7)
                              (LamE
                                 (LocalIndex 8)
                                 (AppE
                                    (AppE
                                       (VarE (LocalIndex 5))
                                       (AppE
                                          (VarE (LocalIndex 6))
                                          (VarE (LocalIndex 7))))
                                    (AppE
                                       (VarE (LocalIndex 6))
                                       (VarE (LocalIndex 8)))))))
                  }
              ])
          ])
