{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |

module Prana.Interpreter.Eval where

import           Control.Monad.Reader
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Prana.Interpreter.Binding
import           Prana.Interpreter.Boxing
import           Prana.Interpreter.PrimOps
import           Prana.Interpreter.Types
import           Prana.Pretty
import           Prana.Types

evalExpr ::
     ReverseIndex
  -> Map GlobalVarId Box
  -> Map LocalVarId Box
  -> Expr
  -> IO Whnf
evalExpr index globals locals0 toplevelexpr = do
  go locals0 toplevelexpr
  where
    go locals expr = do
      whnf <- go' locals expr
      pure whnf
    go' locals =
      \case
        OpAppExpr expOp args typ ->
          case expOp of
            PrimOp primOp ->
              evalPrimOp
                index
                (evalSomeVarId index globals locals)
                (evalBox index globals)
                locals
                primOp
                args
                typ
            OtherOp ->
              error "Unimplemented op type (either custom primop or FFI)."
        LetExpr localBinding expr -> do
          locals' <- bindLocal localBinding locals
          {-putStrLn
            (unlines
               [ "Evaluating let form:"
               , "  Bindings: " ++ show localBinding
               , "  Expression: " ++ show expr
               ])-}
          go locals' expr
        LitExpr lit -> pure (LitWhnf lit)
        ConAppExpr dataConId args _types -> evalCon locals (Con dataConId args)
        AppExpr someVarId args ->
          let loop [] whnf = pure whnf
              loop args0 whnf = do
                case whnf of
                  FunWhnf localsClosure funParams funBody -> do
                    let (closureArgs, remainderArgs) =
                          splitAt (length funParams) args0
                    {-if length args0 < length funParams
                      then putStrLn (unlines ["Not enough arguments to function:"
                                             ,"  Putting these in scope: " ++ show (zip funParams closureArgs)])
                      else pure ()-}
                    locals' <-
                      foldM
                        (\locals' (param, arg) -> do
                           box <- boxArg locals arg
                           pure (M.insert param box locals'))
                        (localsClosure <> locals)
                        (zip funParams closureArgs)
                    {-putStrLn
                      (unlines
                         [ "Entering function:"
                         , "  Params: " ++ show funParams
                         , "  Arguments: " ++ show closureArgs
                         , "  Body: " ++ show funBody
                         , "  Scope: " ++ show locals'
                         ])-}
                    if length args0 < length funParams
                      then do
                        pure
                          (FunWhnf
                             locals'
                             (drop (length closureArgs) funParams)
                             funBody)
                      else do
                        whnf' <- go locals' funBody
                        loop remainderArgs whnf'
                  ConWhnf {} ->
                    if null args
                      then pure whnf
                      else error
                             ("Unexpected arguments for already-saturated data constructor: " ++
                              show whnf ++ ", args were: " ++ show args)
                  _ -> error ("Expected function, but got: " <> show whnf)
           in do whnf <- evalSomeVarId index globals locals someVarId
                 {-putStrLn
                   (unlines
                      [ "Applying function:"
                      , "  Function: " ++ show whnf
                      , "  Arguments: " ++ show args
                      ])-}
                 loop args whnf
        CaseExpr expr caseExprVarId dataAlts
          -- putStrLn (unlines ["Case expression", "  Scrutinee: " ++ show expr])
         -> do
          case dataAlts of
            DataAlts _tyCon alts mdefaultExpr -> do
              (dataConId, boxes) <- evalExprToCon index globals locals expr
              caseExprBox <- boxWhnf (ConWhnf dataConId boxes)
              let locals1 = M.insert caseExprVarId caseExprBox locals
              let loop (DataAlt altDataConId localVarIds rhsExpr:rest) =
                    if dataConId == altDataConId
                      then if length boxes == length localVarIds
                             then do
                               locals' <-
                                 foldM
                                   (\locals' (box, localVarId) -> do
                                      pure (M.insert localVarId box locals'))
                                   locals1
                                   (zip boxes localVarIds)
                               go locals' rhsExpr
                             else error
                                    "Mismatch between number of slots in constructor and pattern."
                      else loop rest
                  loop [] =
                    case mdefaultExpr of
                      Nothing ->
                        error
                          ("(DataAlts) Inexhaustive pattern match: " ++
                           show alts)
                      Just defaultExpr -> go locals1 defaultExpr
               in loop alts
            PrimAlts _primRep litAlts mdefaultExpr -> do
              whnf <- go locals expr
              caseExprBox <- boxWhnf whnf
              let locals1 = M.insert caseExprVarId caseExprBox locals
              case whnf of
                LitWhnf lit ->
                  let loop [] =
                        case mdefaultExpr of
                          Nothing ->
                            error "Inexhaustive primitive pattern match..."
                          Just defaultExpr -> go locals1 defaultExpr
                      loop (litAlt:rest) =
                        if litAltLit litAlt == lit
                          then go locals1 (litAltExpr litAlt)
                          else loop rest
                   in loop litAlts
                _ ->
                  error
                    ("Unexpected whnf for PrimAlts (I'm sure ClosureWhnf will come up here): " ++
                     show whnf)
            MultiValAlts size alts mdefaultExpr -> do
              (dataConId, boxes) <- evalExprToCon index globals locals expr
              caseExprBox <- boxWhnf (ConWhnf dataConId boxes)
              let locals1 = M.insert caseExprVarId caseExprBox locals
              let loop (DataAlt altDataConId localVarIds rhsExpr:rest) = do
                    if dataConId == altDataConId
                      then if length boxes == length localVarIds &&
                              length boxes == size
                             then do
                               locals' <-
                                 foldM
                                   (\locals' (box, localVarId) -> do
                                      pure (M.insert localVarId box locals'))
                                   locals1
                                   (zip boxes localVarIds)
                               go locals' rhsExpr
                             else error
                                    "Mismatch between number of slots in constructor and pattern."
                      else loop rest
                  loop [] =
                    case mdefaultExpr of
                      Nothing ->
                        error
                          ("(MultiValAlts) Inexhaustive pattern match! " ++
                           show size ++
                           " " ++
                           show alts ++ "\n" ++ prettyExpr index toplevelexpr)
                      Just defaultExpr -> go locals1 defaultExpr
               in loop alts
            PolymorphicAlt rhsExpr -> do
              (dataConId, boxes) <- evalExprToCon index globals locals expr
              caseExprBox <- boxWhnf (ConWhnf dataConId boxes)
              let locals1 = M.insert caseExprVarId caseExprBox locals
              go locals1 rhsExpr

evalExprToCon :: ReverseIndex -> Map GlobalVarId Box -> Map LocalVarId Box -> Expr -> IO (DataConId, [Box])
evalExprToCon index globals locals0 expr = do
  whnf <- evalExpr index globals locals0 expr
  case whnf of
    ConWhnf dataConId boxes -> pure (dataConId, boxes)
    FunWhnf{} ->
      error "Unexpected function for data alt case scrutinee."
    LitWhnf {} -> error "Unexpected literal for data alt case scrutinee."

evalBox :: ReverseIndex -> Map GlobalVarId Box -> Box -> IO Whnf
evalBox index globals box = do
  thunk <- readIORef (boxIORef box)
  whnf <-
    case thunk of
      WhnfThunk whnf -> pure whnf
      VariableThunk locals someVarId ->
        evalSomeVarId index globals locals someVarId
      ExpressionThunk locals expr -> evalExpr index globals locals expr
  writeIORef (boxIORef box) (WhnfThunk whnf)
  pure whnf

evalSomeVarId ::
     ReverseIndex -> Map GlobalVarId Box -> Map LocalVarId Box -> SomeVarId -> IO Whnf
evalSomeVarId index globals locals someVarId = do
  whnf <-
    case someVarId of
      SomeLocalVarId localVarId ->
        case M.lookup localVarId locals of
          Nothing ->
            error
              ("Couldn't find local " ++
               show localVarId ++
               ", " ++
               (case M.lookup localVarId (reverseIndexLocals index) of
                  Nothing -> error "Couldn't find name! BUG!"
                  Just name -> displayName name) ++
               "\nIn scope: " ++ show locals)
          Just box -> evalBox index globals box
      SomeGlobalVarId globalVarId ->
        case M.lookup globalVarId globals of
          Nothing ->
            error
              ("Couldn't find global " ++
               case M.lookup globalVarId (reverseIndexGlobals index) of
                 Nothing -> error "Couldn't find name! BUG!"
                 Just name -> displayName name)
          Just box -> evalBox index globals box
      w@WiredInVal {} -> error ("TODO: Wired in: " ++ show w)
  -- putStrLn (prettySomeVarId index someVarId ++ " = " ++ show whnf)
  {-putStrLn (unlines ["Looking up id:"
                    ,"  Id: " ++ show someVarId
                    ,"  Whnf: " ++ show whnf])-}
  pure whnf

evalCon :: Map LocalVarId Box -> Con -> IO Whnf
evalCon locals (Con dataConId args) =
  ConWhnf dataConId <$> traverse (boxArg locals) args
