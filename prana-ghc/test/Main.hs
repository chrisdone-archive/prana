{-# LANGUAGE BangPatterns #-}
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
import           Formatting
import           Formatting.Clock
import qualified GHC
import qualified GHC.Paths
import           Prana.Ghc
import           Prana.Index
import           Prana.Rename
import           Prana.Types
import           System.Clock
import           Weigh

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
                    Just (RhsClosure closure@Closure {closureParams = []}) -> do
                      ghcPrim <- loadLibrary options "ghc-prim"
                      integerGmp <- loadLibrary options "integer-gmp"
                      base <- loadLibrary options "base"
                      let bindings = ghcPrim <> integerGmp <> base <> bindings0
                      -- let bindings = bindings0
                      !globals <-
                        foldM
                          (\globals binding -> bindGlobal binding globals)
                          mempty
                          bindings
                      putStrLn (displayName name ++ " = ")

                      (bytes, gcs, liveBytes, maxByte) <-
                        weighAction
                          (const
                             (do let !rev = (reverseIndex index)
                                 start <- getTime Monotonic
                                 whnf <-
                                   evalExpr
                                     rev
                                     globals
                                     mempty
                                     (closureExpr closure)
                                 end <- getTime Monotonic
                                 fprint (timeSpecs % "\n") start end
                                 printWhnf (reverseIndex index) globals whnf
                                 putStrLn ""))
                          ()

                      putStrLn
                        (reportGroup
                           defaultConfig
                             { configColumns =
                                 [Case, Allocated, GCs, Max]
                             }
                           ""
                           [ Singleton
                               ( Weight
                                   { weightLabel = "Eval"
                                   , weightAllocatedBytes = bytes
                                   , weightGCs = gcs
                                   , weightLiveBytes = liveBytes
                                   , weightMaxBytes = maxByte
                                   }
                               , Nothing)
                           ])
                    Just (RhsCon con) -> do
                      ghcPrim <- loadLibrary options "ghc-prim"
                      integerGmp <- loadLibrary options "integer-gmp"
                      base <- loadLibrary options "base"
                      let bindings = ghcPrim <> integerGmp <> base <> bindings0
                      -- let bindings = bindings0
                      !globals <-
                        foldM
                          (\globals binding -> bindGlobal binding globals)
                          mempty
                          bindings
                      putStrLn
                        (displayName name ++
                         " = " ++ prettyRhs (reverseIndex index) (RhsCon con))
                      putStrLn ("> " ++ displayName name)
                      whnf <- evalCon mempty con
                      printWhnf (reverseIndex index) globals whnf
                      putStrLn ""
                    Just clj ->
                      putStrLn
                        ("The expression should take no arguments: " ++ show clj)
                    Nothing -> putStrLn ("Couldn't find " <> displayName name))
             Left err -> showErrors err))

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
    FunWhnf locals params expr -> do
      putStr "<function>"

deepseqWhnf :: ReverseIndex -> Map GlobalVarId Box -> Whnf -> IO ()
deepseqWhnf index globals =
  \case
    LitWhnf lit -> pure ()
    ConWhnf dataConId boxes -> do
      mapM_
        (\box -> do
           whnf <- evalBox index globals box
           deepseqWhnf index globals whnf)
        boxes
    FunWhnf locals params expr -> do
      pure ()

data Whnf
  = LitWhnf Lit
  | ConWhnf DataConId [Box]
  | FunWhnf (Map LocalVarId Box) [LocalVarId] Expr
  deriving (Show)

newtype Box = Box { boxIORef :: IORef Thunk }
instance Show Box where show _ = "Box"

data Thunk
  = VariableThunk (Map LocalVarId Box) SomeVarId
  | ExpressionThunk (Map LocalVarId Box) Expr
  | WhnfThunk Whnf

data Env =
  Env
    { envLocals :: Map LocalVarId Box
    , envGlobals :: Map GlobalVarId Box
    }

evalExpr :: ReverseIndex -> Map GlobalVarId Box -> Map LocalVarId Box -> Expr -> IO Whnf
evalExpr index globals locals0 = do
  go locals0
  where
    go locals expr = do
      whnf <- go' locals expr
      pure whnf
    go' locals =
      \case
        OpAppExpr op args typ ->
          case op of
            PrimOp primOp ->
              case primOp of
                UnknownPrimOp string ->
                  error
                    ("Unimplemented primop: " ++
                     string ++
                     " (type: " ++ show typ ++ "), args were: " ++ show args)
                IntEqOp ->
                  case args of
                    [arg1, arg2] -> do
                      i <- evalIntArg index globals locals arg1
                      i2 <- evalIntArg index globals locals arg2
                      -- print (show i ++ " ==# " ++ show i2)
                      let !r = i == i2
                      pure
                        (LitWhnf
                           (IntLit
                              (if r
                                 then 1
                                 else 0)))
                    _ -> error ("Invalid arguments to IntNegOp: " ++ show args)
                IntAddOp ->
                  case args of
                    [arg1, arg2] -> do
                      i <- evalIntArg index globals locals arg1
                      i2 <- evalIntArg index globals locals arg2
                      -- print (show i ++ " +# " ++ show i2)
                      let !r = i + i2
                      pure (LitWhnf (IntLit r))
                    _ -> error ("Invalid arguments to IntAddOp: " ++ show args)
                IntSubOp ->
                  case args of
                    [arg1, arg2] -> do
                      i <- evalIntArg index globals locals arg1
                      i2 <- evalIntArg index globals locals arg2
                      -- print (show i ++ " +# " ++ show i2)
                      let !r = i - i2
                      pure (LitWhnf (IntLit r))
                    _ -> error ("Invalid arguments to IntSubOp: " ++ show args)
                IntNegOp ->
                  case args of
                    [arg] -> do
                      i <- evalIntArg index globals locals arg
                      let !i' = negate i
                      pure (LitWhnf (IntLit i'))
                    _ -> error ("Invalid arguments to IntNegOp: " ++ show args)
                TagToEnumOp ->
                  case args of
                    [arg] -> do
                      i <- evalIntArg index globals locals arg
                      {-print (show i ++ " tagToEnum => " ++ ((case i of
                                                               0 -> "False"
                                                               _ -> "True")))-}
                      case typ of
                        BoolType ->
                          pure
                            (ConWhnf
                               (case i of
                                  0 -> reverseIndexFalse index
                                  _ -> reverseIndexTrue index)
                               [])
                        _ -> error "Unknown type for tagToEnum."
                    _ ->
                      error ("Invalid arguments to TagToEnumOp: " ++ show args)
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
                      then do pure
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
        CaseExpr expr caseExprVarId dataAlts -> do
          -- putStrLn (unlines ["Case expression", "  Scrutinee: " ++ show expr])
          case dataAlts of
            DataAlts _tyCon alts mdefaultExpr -> do
              (dataConId, boxes) <- evalExprToCon index globals locals expr
              caseExprBox <- boxWhnf locals (ConWhnf dataConId boxes)
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
                      Nothing -> error "Inexhaustive pattern match!"
                      Just defaultExpr -> go locals1 defaultExpr
               in loop alts
            PrimAlts primRep litAlts mdefaultExpr -> do
              whnf <- go locals expr
              caseExprBox <- boxWhnf locals whnf
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
            _ -> error ("TODO: implement alts:\n" <> prettyAlts index dataAlts)

evalExprToCon :: ReverseIndex -> Map GlobalVarId Box -> Map LocalVarId Box -> Expr -> IO (DataConId, [Box])
evalExprToCon index globals locals0 expr = do
  whnf <- evalExpr index globals locals0 expr
  case whnf of
    ConWhnf dataConId boxes -> pure (dataConId, boxes)
    FunWhnf locals params expr ->
      error "Unexpected function for data alt case scrutinee."
    LitWhnf {} -> error "Unexpected literal for data alt case scrutinee."

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
        [ prettySomeVarId index someVarId
        , prettyList (map (prettyArg index) args)
        ]
    ConAppExpr dataConId args _ty ->
      node
        "ConAppExpr"
        [ prettyDataConId index dataConId
        , prettyList (map (prettyArg index) args)
        ]
    OpAppExpr op args primType ->
      node
        "OpAppExpr"
        [show op, prettyList (map (prettyArg index) args), show primType]
    CaseExpr expr localVarId alts ->
      node
        "CaseExpr"
        [ prettyExpr index expr
        , prettyLocalVar index localVarId
        , prettyAlts index alts
        ]
    LetExpr binding expr -> node "LetExpr[TODO]" [prettyExpr index expr]
    LitExpr lit -> show lit

prettyAlts :: ReverseIndex -> Alts -> [Char]
prettyAlts index =
  \case
    PolymorphicAlt e -> node "PolymorphicAlt" [prettyExpr index e]
    DataAlts tyCon dataAlts mexpr ->
      node
        "DataAlts"
        [ prettyTyCon tyCon
        , prettyList (map prettyDataAlt dataAlts)
        , maybe "Nothing" (prettyExpr index) mexpr
        ]
    MultiValAlts i dataAlts mexpr ->
      node
        "MultiValAlts"
        [ show i
        , prettyList (map prettyDataAlt dataAlts)
        , maybe "Nothing" (prettyExpr index) mexpr
        ]
    PrimAlts primRep litAlts mexpr ->
      node
        "PrimAlts"
        [ show primRep
        , prettyList (map prettyLitAlt litAlts)
        , maybe "Nothing" (prettyExpr index) mexpr
        ]
  where
    prettyTyCon = show
    prettyLitAlt litAlt =
      node
        "LitAlt"
        [ show (litAltLit litAlt)
        , prettyList (map (prettyLocalVar index) (litAltBinders litAlt))
        , prettyExpr index (litAltExpr litAlt)
        ]
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
     Nothing -> case dataConId of
                  WiredInCon wiredIn -> show wiredIn
                  _ -> error ("Couldn't find name! BUG!" ++ show dataConId)
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

bindLocal :: LocalBinding -> Map LocalVarId Box -> IO (Map LocalVarId Box)
bindLocal localBinding locals =
  case localBinding of
    LocalNonRec var rhs -> do
      box <- boxRhs locals rhs
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
    VarArg someVarId -> fmap Box (newIORef (VariableThunk locals someVarId))
    LitArg lit -> fmap Box (newIORef (WhnfThunk (LitWhnf lit)))

boxRhs :: Map LocalVarId Box -> Rhs -> IO Box
boxRhs locals =
  \case
    RhsCon (Con dataConId args) -> do
      boxes <- traverse (boxArg locals) args
      fmap Box (newIORef (WhnfThunk (ConWhnf dataConId boxes)))
    RhsClosure closure ->
      if null (closureParams closure)
        then fmap Box (newIORef (ExpressionThunk locals (closureExpr closure)))
        else fmap
               Box
               (newIORef
                  (WhnfThunk
                     (FunWhnf
                        locals
                        (closureParams closure)
                        (closureExpr closure))))

boxWhnf :: Map LocalVarId Box -> Whnf -> IO Box
boxWhnf locals whnf =
  fmap Box (newIORef (WhnfThunk whnf))

evalIntArg :: ReverseIndex -> Map GlobalVarId Box -> Map LocalVarId Box -> Arg -> IO Integer
evalIntArg index globals locals =
  \case
    LitArg (IntLit !i) -> pure i
    LitArg lit -> error ("Invalid lit rep: " ++ show lit)
    VarArg someVarId -> do
      whnf <- evalSomeVarId index globals locals someVarId
      case whnf of
        LitWhnf (IntLit !i) -> pure i
        LitWhnf lit -> error ("Invalid lit rep: " ++ show lit)
        _ ->
          error
            ("Unexpected whnf for evalIntArg (I'm sure ClosureWhnf will come up here): " ++
             show whnf)
