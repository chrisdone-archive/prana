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
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified DynFlags
import qualified GHC
import qualified GHC.Paths
import           Prana.Ghc
import           Prana.Index
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
             Right (index, bindings0) -> do
               liftIO (print bindings0)
               let name =
                     Name
                       { namePackage = "main"
                       , nameModule = "Fib"
                       , nameName = "it"
                       , nameUnique = Exported
                       }
               liftIO
                 (case lookupGlobalBindingRhsByName index bindings0 name of
                    Just (RhsClosure closure@Closure{closureParams = []}) -> do
                      ghcPrim <- loadLibrary options "ghc-prim"
                      integerGmp <- loadLibrary options "integer-gmp"
                      base <- loadLibrary options "base"
                      let bindings = ghcPrim <> integerGmp <> base <> bindings0
                      -- let bindings = bindings0
                      globals <- foldM (\globals binding -> bindGlobal binding globals)
                                       mempty bindings
                      putStr (displayName name ++ " = ")
                      whnf <- evalExpr (reverseIndex index) globals mempty (closureExpr closure)
                      printWhnf (reverseIndex index) globals whnf
                      putStrLn ""
                    Just (RhsCon con) -> do
                      ghcPrim <- loadLibrary options "ghc-prim"
                      integerGmp <- loadLibrary options "integer-gmp"
                      base <- loadLibrary options "base"
                      let bindings = ghcPrim <> integerGmp <> base <> bindings0
                      -- let bindings = bindings0
                      globals <- foldM (\globals binding -> bindGlobal binding globals)
                                       mempty bindings
                      putStrLn (displayName name ++ " = " ++ prettyRhs (reverseIndex index) (RhsCon con))
                      putStrLn ("> " ++ displayName name)
                      whnf <- evalCon mempty con
                      printWhnf (reverseIndex index) globals whnf
                      putStrLn ""
                    Just clj -> putStrLn ("The expression should take no arguments: " ++ show clj)
                    Nothing -> putStrLn ("Couldn't find " <> displayName name))
             Left err -> showErrors err))

printWhnf :: ReverseIndex -> Map GlobalVarId Box -> Whnf -> IO ()
printWhnf index globals =
  \case
    LitWhnf lit -> putStr (show lit)
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
    ClosureWhnf closure -> do
      whnf <- evalExpr index globals mempty (closureExpr closure)
      printWhnf index globals whnf

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

evalExpr :: ReverseIndex -> Map GlobalVarId Box -> Map LocalVarId Box -> Expr -> IO Whnf
evalExpr index globals locals0 = go locals0
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
          whnf <- evalSomeVarId index globals locals someVarId
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
        e@CaseExpr {} -> error ("TODO: implement case:\n" <> prettyExpr index e)


prettyRhs :: ReverseIndex -> Rhs -> String
prettyRhs index =
  \case
    RhsCon con -> node "RhsCon" [prettyCon index con]

prettyCon :: ReverseIndex -> Con -> [Char]
prettyCon index (Con dataConId args) =
  node
    "Con"
    [prettyDataConId index dataConId, prettyList (map (prettyArg index) args)]

prettyExpr :: ReverseIndex -> Expr -> String
prettyExpr index =
  \case
    AppExpr someVarId args ->
      node
        "AppExpr"
        (prettySomeVarId index someVarId : map (prettyArg index) args)
    ConAppExpr dataConId args _ty ->
      node
        "ConAppExpr"
        (prettyDataConId index dataConId : map (prettyArg index) args)
    OpAppExpr {} -> "OpAppExpr"
    CaseExpr expr localVarId alts ->
      node
        "CaseExpr"
        [ prettyExpr index expr
        , prettyLocalVar index localVarId
        , prettyAlts alts
        ]
    LetExpr binding expr -> node "LetExpr[TODO]" [prettyExpr index expr]
    LitExpr lit -> show lit
  where
    prettyAlts =
      \case
        PolymorphicAlt e -> node "PolymorphicAlt" [prettyExpr index e]
        DataAlts tyCon dataAlts mexpr ->
          node
            "DataAlts"
            [ prettyTyCon tyCon
            , prettyList (map prettyDataAlt dataAlts)
            , maybe "Nothing" (prettyExpr index) mexpr
            ]
        MultiValAlts int dataAlts mexpr ->
          node
            "MultiValAlts"
            [ show int
            , prettyList (map prettyDataAlt dataAlts)
            , maybe "Nothing" (prettyExpr index) mexpr
            ]
        PrimAlts primRep litAlts mexpr -> node "PrimAlts[TODO]" []
    prettyTyCon = show
    prettyDataAlt dataAlt =
      node
        "DataAlt"
        [ prettyDataConId index (dataAltCon dataAlt)
        , prettyList (map (prettyLocalVar index) (dataAltBinders dataAlt))
        , prettyExpr index (dataAltExpr dataAlt)
        ]

prettyLocalVar :: ReverseIndex -> LocalVarId -> String
prettyLocalVar index localVarId =
  case M.lookup localVarId (reverseIndexLocals index) of
    Nothing -> error "Couldn't find name! BUG!"
    Just name -> show (displayName name)

prettySomeVarId :: ReverseIndex -> SomeVarId -> String
prettySomeVarId index =
  \case
    SomeLocalVarId localVarId -> prettyLocalVar index localVarId
    SomeGlobalVarId globalVarId ->
      (case M.lookup globalVarId (reverseIndexGlobals index) of
         Nothing -> error "Couldn't find name! BUG!"
         Just name -> show (displayName name))
    w@WiredInVal {} -> error ("TODO: Wired in: " ++ show w)

prettyList :: [[Char]] -> [Char]
prettyList xs = "[" ++ intercalate "\n," (map indent1 xs) ++ "]"

prettyDataConId :: ReverseIndex -> DataConId -> String
prettyDataConId index dataConId =
  (case M.lookup dataConId (reverseIndexDataCons index) of
     Nothing -> error "Couldn't find name! BUG!"
     Just name -> show (displayName name))

prettyArg :: ReverseIndex -> Arg -> [Char]
prettyArg index =
  \case
    VarArg someVarId -> node "VarArg" [prettySomeVarId index someVarId]
    LitArg lit -> node "LitArg" [show lit]

node :: [Char] -> [String] -> [Char]
node a rest = "(" ++ intercalate "\n" (a : map indent rest) ++ ")"

indent :: String -> [Char]
indent = intercalate "\n" . map ("  " ++) . lines

indent1 :: String -> [Char]
indent1 = intercalate "\n" . map (" " ++) . lines

evalBox :: ReverseIndex -> Map GlobalVarId Box -> Box -> IO Whnf
evalBox index globals box = do
  thunk <- readIORef (boxIORef box)
  case thunk of
    WhnfThunk whnf -> pure whnf
    UnevaluatedThunk locals someVarId -> evalSomeVarId index globals locals someVarId

evalSomeVarId ::
     ReverseIndex -> Map GlobalVarId Box -> Map LocalVarId Box -> SomeVarId -> IO Whnf
evalSomeVarId index globals locals someVarId =
  case someVarId of
    SomeLocalVarId localVarId ->
      case M.lookup localVarId locals of
        Nothing ->
          error
            ("Couldn't find local " ++
             case M.lookup localVarId (reverseIndexLocals index) of
               Nothing -> error "Couldn't find name! BUG!"
               Just name -> displayName name)
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
