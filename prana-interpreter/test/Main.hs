{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Print STG in GHC 8.4.3.

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Prana.Ghc
import           Prana.Index
import           Prana.Interpreter
import           Prana.Interpreter.Boxing
import           Prana.Rename
import           Prana.Types
import           Test.Hspec (runIO, shouldReturn, it, describe, hspec, Spec)

main :: IO ()
main =
  hspec spec

spec :: Spec
spec =
  describe
    "Fib"
    (do (options, std, index) <-
          runIO
            (do options <- getOptions
                std <- loadStandardPackages options
                index <- liftIO (readIndex (optionsIndexPath options))
                pure (options, std, index))
        dataConI# <- getDataConI# index
        it
          "Double"
          (do steps <-
                compileAndRun
                  index
                  options
                  std
                  "test/assets/DoubleTest.hs"
                  "DoubleTest"
                  PureMode
              shouldReturn
                (runConduit (steps .| CL.consume))
                [ BeginConStep
                    (DataConId
                       (TypeId {typeIdInt = 17})
                       (ConIndex {conIndexInt = 0}))
                , LitStep (DoubleLit 2.302585092994046)
                , EndConStep
                ])
        it
          "Iterative"
          (do steps <-
                compileAndRun
                  index
                  options
                  std
                  "test/assets/FibIterative.hs"
                  "FibIterative"
                  PureMode
              shouldReturn
                (runConduit (steps .| CL.consume))
                [ BeginConStep dataConI#
                , LitStep (IntLit 12586269025)
                , EndConStep
                ])
        it
          "Codata"
          (do steps <-
                compileAndRun
                  index
                  options
                  std
                  "test/assets/FibCodata.hs"
                  "FibCodata"
                  PureMode
              shouldReturn
                (runConduit (steps .| CL.consume))
                [ BeginConStep dataConI#
                , LitStep (IntLit 12586269025)
                , EndConStep
                ])
        it
          "CharTest"
          (do steps <-
                compileAndRun
                  index
                  options
                  std
                  "test/assets/CharTest.hs"
                  "CharTest"
                  PureMode
              shouldReturn
                (runConduit (steps .| CL.consume))
                [ BeginConStep
                    (DataConId
                       (TypeId {typeIdInt = 23})
                       (ConIndex {conIndexInt = 1}))
                , BeginConStep
                    (DataConId
                       (TypeId {typeIdInt = 21})
                       (ConIndex {conIndexInt = 0}))
                , LitStep (CharLit '1')
                , EndConStep
                , BeginConStep
                    (DataConId
                       (TypeId {typeIdInt = 23})
                       (ConIndex {conIndexInt = 1}))
                , BeginConStep
                    (DataConId
                       (TypeId {typeIdInt = 21})
                       (ConIndex {conIndexInt = 0}))
                , LitStep (CharLit '2')
                , EndConStep
                , BeginConStep
                    (DataConId
                       (TypeId {typeIdInt = 23})
                       (ConIndex {conIndexInt = 1}))
                , BeginConStep
                    (DataConId
                       (TypeId {typeIdInt = 21})
                       (ConIndex {conIndexInt = 0}))
                , LitStep (CharLit '3')
                , EndConStep
                , BeginConStep
                    (DataConId
                       (TypeId {typeIdInt = 23})
                       (ConIndex {conIndexInt = 0}))
                , EndConStep
                , EndConStep
                , EndConStep
                , EndConStep
                ])
        it
          "ArrayTest"
          (do steps <-
                compileAndRun
                  index
                  options
                  std
                  "test/assets/ArrayTest.hs"
                  "ArrayTest"
                  IOMode
              shouldReturn (runConduit (steps .| CL.consume)) []))

getDataConI# :: Applicative f => Index -> f DataConId
getDataConI# index =
  case M.lookup
         (Name
            { namePackage = "ghc-prim"
            , nameModule = "GHC.Types"
            , nameName = "I#"
            , nameUnique = Exported
            })
         (indexDataCons index) of
    Nothing -> error "Couldn't find constructor."
    Just dataConId -> pure dataConId

data RunMode = PureMode | IOMode

-- | Compile and run left-to-right evaluation of the complete data structure.
compileAndRun ::
     Index
  -> Options
  -> [GlobalBinding]
  -> String
  -> ByteString
  -> RunMode
  -> IO (ConduitT () Step IO ())
compileAndRun index0 options std fileName moduleName runMode =
  runGhc
    (do setModuleGraph [fileName]
        libraryGlobals <- foldM bindGlobal mempty std
        result <- compileModuleGraph options index0
        case result of
          Left err -> do
            showErrors err
            error "Bailing out."
          Right (index, sourceGlobals) -> do
            let name =
                  Name
                    { namePackage = "main"
                    , nameModule = moduleName
                    , nameName = "it"
                    , nameUnique = Exported
                    }
            liftIO
              (do globals <- foldM bindGlobal libraryGlobals sourceGlobals
                  case lookupGlobalBindingRhsByName index sourceGlobals name of
                    Just (RhsClosure closure@Closure {closureParams = []}) -> do
                      whnf <-
                        evalExpr
                          (reverseIndex index)
                          globals
                          mempty
                          (closureExpr closure)
                      pure (stepSource (reverseIndex index) globals whnf)
                    Just (RhsCon con) -> do
                      whnf <- evalCon mempty con
                      pure (stepSource (reverseIndex index) globals whnf)
                    Just clj@(RhsClosure closure@Closure{closureParams = [stateS]}) ->
                      case runMode of
                        PureMode -> error
                                       ("The expression should take no arguments: " ++ show clj)
                        IOMode -> do
                          stateBox <- boxWhnf StateWhnf
                          whnf <-
                            evalExpr
                              (reverseIndex index)
                              globals
                              (M.singleton stateS stateBox)
                              (closureExpr closure)
                          pure (stepSource (reverseIndex index) globals whnf)
                    Nothing -> error ("Couldn't find " <> displayName name)
                    _ -> error "Unexpected top-level format."))

data Step
  = LitStep Lit
  | BeginConStep DataConId
  | EndConStep
  | FunStep (Map LocalVarId Box) [LocalVarId] (Expr)
  deriving (Show, Eq)

stepSource ::
     MonadIO m
  => ReverseIndex
  -> Map GlobalVarId Box
  -> Whnf
  -> ConduitT () Step m ()
stepSource index globals =
  \case
    LitWhnf lit -> yield (LitStep lit)
    ConWhnf dataConId boxes -> do
      yield (BeginConStep dataConId)
      mapM_
        (\box -> do
           whnf <- liftIO (evalBox index globals box)
           stepSource index globals whnf)
        boxes
      yield EndConStep
    FunWhnf locals params expr -> yield (FunStep locals params expr)

printWhnf :: ReverseIndex -> Map GlobalVarId Box -> Whnf -> IO ()
printWhnf index globals =
  \case
    LitWhnf lit -> putStr ("(" ++ show lit ++ ")")
    ConWhnf dataConId boxes -> do
      putStr "("
      putStr (case M.lookup dataConId (reverseIndexDataCons index) of
                Nothing -> error "Couldn't find name! BUG!"
                Just name -> displayName name)
      mapM_
        (\(i, box) -> do
           when (i/=length boxes) (putStr " ")
           whnf <- evalBox index globals box
           printWhnf index globals whnf)
        (zip [0 ..] boxes)
      putStr ")"
    FunWhnf {} -> do
      putStr "<function>"

deepseqWhnf :: ReverseIndex -> Map GlobalVarId Box -> Whnf -> IO ()
deepseqWhnf index globals =
  \case
    LitWhnf{} -> pure ()
    ConWhnf _dataConId boxes -> do
      mapM_
        (\box -> do
           whnf <- evalBox index globals box
           deepseqWhnf index globals whnf)
        boxes
    FunWhnf{} -> do
      pure ()
