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
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified DynFlags
import qualified GHC
import qualified GHC.Paths
import           Prana.Ghc
import           Prana.Rename
import           Prana.Types

main :: IO ()
main =
  GHC.defaultErrorHandler
    DynFlags.defaultFatalMessager
    DynFlags.defaultFlushOut
    (GHC.runGhc
       (Just GHC.Paths.libdir)
       (do dflags <- GHC.getSessionDynFlags
           _ <- GHC.setSessionDynFlags dflags
           target <- GHC.guessTarget "Fib.hs" Nothing
           GHC.setTargets [target]
           _ <- GHC.load GHC.LoadAllTargets
           options <- liftIO getOptions
           result <- compileModuleGraph options
           case result of
             Right (index, bindings) -> do
               let name =
                     Name
                       { namePackage = "main"
                       , nameModule = "Fib"
                       , nameName = "it"
                       , nameUnique = Exported
                       }
               liftIO
                 (case lookupGlobalBindingRhsByName index bindings name of
                    Just (RhsClosure closure@Closure{closureParams = []}) -> do
                      -- ghcPrim <- loadLibrary options "ghc-prim"
                      -- integerGmp <- loadLibrary options "integer-gmp"
                      -- base <- loadLibrary options "base"
                      print closure
                      print (lookupGlobalBindingRhsById bindings (GlobalVarId 56634))
                      globals <- foldM (\globals binding -> bindGlobal binding globals)
                                       mempty bindings
                      evalExpr globals mempty (closureExpr closure) >>= print
                    Just (RhsCon con) -> do
                      -- ghcPrim <- loadLibrary options "ghc-prim"
                      -- integerGmp <- loadLibrary options "integer-gmp"
                      -- base <- loadLibrary options "base"
                      print con
                      print (lookupGlobalBindingRhsById bindings (GlobalVarId 56634))
                      globals <- foldM (\globals binding -> bindGlobal binding globals)
                                       mempty bindings
                      evalCon mempty con >>= print
                    Just clj -> putStrLn ("The expression should take no arguments: " ++ show clj)
                    Nothing -> putStrLn ("Couldn't find " <> displayName name))
             Left err -> showErrors err))

data Whnf
  = LitWhnf Lit
  | ConWhnf DataConId [Box]
  | ClosureWhnf Closure
  deriving (Show)

newtype Box = Box { boxIORef :: IORef Thunk }
instance Show Box where show _ = "Box"

data Thunk
  = UnevaluatedThunk (Map LocalVarId Box) SomeVarId
  | WhnfThunk Whnf

data Env =
  Env
    { envLocals :: Map LocalVarId Box
    , envGlobals :: Map GlobalVarId Box
    }

evalExpr :: Map GlobalVarId Box -> Map LocalVarId Box -> Expr -> IO Whnf
evalExpr globals locals0 = go locals0
  where
    go locals =
      \case
        e@(OpAppExpr {}) -> error ("TODO: " <> show e)
        LetExpr localBinding expr -> do
          locals' <- bindLocal localBinding locals
          go locals' expr
        LitExpr lit -> pure (LitWhnf lit)
        ConAppExpr dataConId args _types -> evalCon locals (Con dataConId args)
        AppExpr someVarId args -> do
          whnf <- evalSomeVarId globals locals someVarId
          case whnf of
            ClosureWhnf closure
              | not (null (closureParams closure)) -> do
                locals' <-
                  foldM
                    (\locals' (param, arg) -> do
                       box <- boxArg locals arg
                       pure (M.insert param box locals'))
                    locals
                    (zip (closureParams closure) args)
                go locals' (closureExpr closure)
            _ -> error ("Expected function, but got: " <> show whnf)

evalBox :: Map GlobalVarId Box -> Box -> IO Whnf
evalBox globals box = do
  thunk <- readIORef (boxIORef box)
  case thunk of
    WhnfThunk whnf -> pure whnf
    UnevaluatedThunk locals someVarId -> evalSomeVarId globals locals someVarId

evalSomeVarId ::
     Map GlobalVarId Box -> Map LocalVarId Box -> SomeVarId -> IO Whnf
evalSomeVarId globals locals someVarId =
  case someVarId of
    SomeLocalVarId localVarId ->
      case M.lookup localVarId locals of
        Nothing -> error ("Couldn't find local " ++ show localVarId)
        Just box -> evalBox globals box
    SomeGlobalVarId globalVarId ->
      case M.lookup globalVarId globals of
        Nothing -> error ("Couldn't find global " ++ show globalVarId)
        Just box -> evalBox globals box
    w@WiredInVal{} -> error ("TODO: Wired in: " ++ show w)

evalCon :: Map LocalVarId Box -> Con -> IO Whnf
evalCon locals (Con dataConId args) =
  ConWhnf dataConId <$> traverse (boxArg locals) args

bindLocal :: LocalBinding -> Map LocalVarId Box -> IO (Map LocalVarId Box)
bindLocal localBinding locals =
  case localBinding of
    LocalNonRec var rhs -> mdo
      box <- boxRhs locals' rhs
      let locals' = M.insert var box locals
      pure locals'
    LocalRec pairs -> mdo
      locals' <-
        foldM
          (\acc (var, rhs) -> do
             box <- boxRhs locals' rhs
             pure (M.insert var box acc))
          locals
          pairs
      pure locals'

bindGlobal :: GlobalBinding -> Map GlobalVarId Box -> IO (Map GlobalVarId Box)
bindGlobal globalBinding globals =
  case globalBinding of
    GlobalNonRec var rhs -> do
      box <- boxRhs mempty rhs
      let globals' = M.insert var box globals
      pure globals'
    GlobalRec pairs -> do
      globals' <-
        foldM
          (\acc (var, rhs) -> do
             box <- boxRhs mempty rhs
             pure (M.insert var box acc))
          globals
          pairs
      pure globals'

boxArg :: Map LocalVarId Box -> Arg -> IO Box
boxArg locals =
  \case
    VarArg someVarId -> fmap Box (newIORef (UnevaluatedThunk locals someVarId))
    LitArg lit -> fmap Box (newIORef (WhnfThunk (LitWhnf lit)))

boxRhs :: Map LocalVarId Box -> Rhs -> IO Box
boxRhs locals =
  \case
    RhsCon (Con dataConId args) -> do
      boxes <- traverse (boxArg locals) args
      fmap Box (newIORef (WhnfThunk (ConWhnf dataConId boxes)))
    RhsClosure closure -> fmap Box (newIORef (WhnfThunk (ClosureWhnf closure)))
