{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- |

module Prana.Interpret where

import           Control.Arrow
import           Control.Concurrent
import           Control.Exception (Exception, throw)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Builder as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Typeable
import           Data.IORef
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Foreign.Marshal
import           GHC.Base
import           GHC.Exts
import           Prana.Types

-- | An environment to evaluate expressions in.
data Env = Env
  { envGlobals :: !(IORef (Map ByteString Exp))
  , envLets :: !(Map Id Exp)
  , envMethods :: !(IORef (Map ByteString Int))
  , envDepth :: !Int
  }

-- | Evaluation computation.
newtype Eval a = Eval
  { runEval :: ReaderT Env IO a
  } deriving (MonadIO, Monad, Applicative, Functor, MonadReader Env)

-- | A interpreter error in the interpreter.
data InterpreterError
  = TypeError TypeError
  | NotInScope Id
  | FailedPatternMatch WHNF [Alt]
  deriving (Show, Typeable)
instance Exception InterpreterError

-- | A type error in the interpreter.
data TypeError
  = NotAFunction WHNF
  | NotAnInstanceDictionary Id WHNF
  | MissingDictionaryMethod Id Int WHNF
  deriving (Show, Typeable)

-- | An expression evaluated to weak head normal form.
data WHNF
  = OpWHNF Op [WHNF]
  | PrimWHNF !Prim
  | IntegerWHNF !Integer
  | ConWHNF !Id ![Exp]
  | LamWHNF !Id !Exp
  | LabelWHNF
  | CoercionWHNF
  | TypWHNF !Typ
  | LetWHNF !Bind !WHNF
  | MethodWHNF !Id !Int
  deriving (Show)

instance Pretty WHNF where
  pretty =
    \case
      LamWHNF i e -> "(\\" <> pretty i <> " -> " <> pretty e <> ")"
      LetWHNF _ e -> "(let in " <> pretty e <> ")"
      TypWHNF (Typ ty) -> "Type[" <> L.byteString ty <> "]"
      CoercionWHNF {} -> "Coercion"
      OpWHNF op ws ->
        "(" <> pretty op <> " " <>
        mconcat (intersperse ", " (map pretty ws)) <>
        ")"
      MethodWHNF op int ->
        pretty op <> "[Method][Idx=" <> L.byteString (S8.pack (show int) <> "]")
      PrimWHNF x -> pretty x
      LabelWHNF -> "LabelWHNF"
      IntegerWHNF i -> "" <> L.byteString (S8.pack (show i)) <> "[Integer]"
      ConWHNF con ws ->
        "(" <> pretty con <> "[Con]" <>
        (if null ws
           then "["
           else " [") <>
        mconcat (intersperse ", " (map pretty ws)) <>
        "])"

-- | A primitive value.
data Prim
  = CharPrim !Char
  | AddrPrim !Addr
  | FloatPrim !Float
  | DoublePrim !Double
  | IntPrim !Int
  | WordPrim !Word
  | ThreadIdPrim !ThreadId
  deriving (Show)

instance Pretty Prim where pretty = L.byteString . S8.pack . show

-- | Some address from GHC.Prim.
data Addr = Addr !Addr#
instance Show Addr where
  show (Addr a) = show (I# (addr2Int# a))

data Op = Op
  { opArity :: {-# UNPACK #-} !Int
  , opName :: {-# UNPACK #-} !ByteString
  } deriving (Show)

instance Pretty Op where
  pretty (Op arity name) =
    L.byteString name <> "[PrimOp,arity=" <> (L.byteString . S8.pack . show $ arity) <>
    "]"

-- | Run the interpreter on the given expression.
runInterpreter :: Map ByteString Exp -> Map ByteString Int -> Exp -> IO WHNF
runInterpreter globals methodIndices e = do
  ref <- newIORef globals
  ref2 <- newIORef methodIndices
  runReaderT (runEval (whnfExp e)) (Env ref mempty ref2 0)

-- | Evaluate the expression to WHNF and no further.
whnfExp :: Exp -> Eval WHNF
whnfExp e0 = do
  depth <- asks envDepth
  let indent = replicate (fromIntegral depth) ' '
      out v = when True (liftIO (S8.putStrLn (S8.pack (indent ++ v))))
  out ("Eval: " ++ L8.unpack (L.toLazyByteString (pretty e0)))
  r <- local (\e -> e {envDepth = envDepth e + 2}) (go e0)
  out ("Done: " ++ L8.unpack (L.toLazyByteString (pretty r)))
  pure r
  where
    go
        -- No-op, lambdas are self-evaluating:
     =
      \case
        LamE True _ e -> go e
        LamE _ i e -> pure (LamWHNF i e)
        -- No-op, types are self-evaluating:
        TypE ty -> pure (TypWHNF ty)
        -- No-op, coerciones are self-evaluating:
        CoercionE -> pure CoercionWHNF
        -- Skip over ticks:
        TickE e -> whnfExp e
        -- Skip over casts:
        CastE e -> whnfExp e
        -- Lookup globals, primitives and lets:
        VarE l -> whnfId l
        -- Evaluate the body of a let, put the binding in scope:
        LetE bind e -> whnfLet bind e
        -- Produce a primitive/runtime value from the literal:
        LitE l -> whnfLit l
        AppE f arg -> whnfApp f arg
        -- Case analysis.
        CaseE e v ty alts -> whnfCase e v ty alts

-- | Evaluate an application to WHNF.
--
-- * If @f@ is a lambda, we beta substitute the argument and evaluate the body.
-- * If @f@ is a data constructor, just return it with the new argument in the arg list.
-- * If @f@ is an operator, reduce the arguments until saturated, then run it.
whnfApp :: Exp -> Exp -> Eval WHNF
whnfApp f arg =
  case arg of
    TypE {} -> do
      result <- whnfExp f
      case result of
        OpWHNF op args -> whnfOp op args arg
        _ -> pure result
    _ -> do
      result <- whnfExp f
      case result of
        LamWHNF v body -> whnfExp (betaSubstitute v arg body)
        OpWHNF op args -> whnfOp op args arg
        ConWHNF i args -> pure (ConWHNF i (args ++ [arg]))
        MethodWHNF i index -> whnfMethod i index arg
        _ -> throw (TypeError (NotAFunction result))

-- | Given an index, force the (what should be) dictionary data
-- constructor's Nth argument, yielding the instance method's code.
whnfMethod :: Id -> Int -> Exp -> Eval WHNF
whnfMethod methodid index dict = do
  result <- whnfExp dict
  if index == -1
    then pure result
    else case result of
           ConWHNF _id args ->
             case lookup index (zip [0 ..] args) of
               Just whnf -> whnfExp whnf
               Nothing ->
                 throw
                   (TypeError (MissingDictionaryMethod methodid index result))
           _ -> throw (TypeError (NotAnInstanceDictionary methodid result))

-- | Force the arguments to WHNF until fully saturated (has all args),
-- then run it.
whnfOp :: Op -> [WHNF] -> Exp -> Eval WHNF
whnfOp op args0 arg = do
  whnf <- whnfExp arg
  let args = args0 ++ [whnf]
   in if length args == opArity op
        then case op of
               Op {opName = "ghc-prim:GHC.Prim.-#"}
                 | [PrimWHNF (IntPrim i), PrimWHNF (IntPrim j)] <- args ->
                   pure (PrimWHNF (IntPrim (i - j)))
               Op {opName = "ghc-prim:GHC.Prim.+#"}
                 | [PrimWHNF (IntPrim i), PrimWHNF (IntPrim j)] <- args ->
                   pure (PrimWHNF (IntPrim (i + j)))
               Op {opName = "ghc-prim:GHC.Prim.<#"}
                 | [PrimWHNF (IntPrim (I# i)), PrimWHNF (IntPrim (I# j))] <-
                    args -> pure (PrimWHNF (IntPrim (I# (i <# j))))
               Op {opName = "ghc-prim:GHC.Prim.tagToEnum#"}
                 | [TypWHNF (Typ _ty), PrimWHNF (IntPrim 0)] <- args ->
                   error "TODO: Implement tagToEnum#"
               _ ->
                 error
                   ("Primop is saturated, apply: " ++
                    show op ++ " with args: " ++ show args)
        else pure (OpWHNF op args)

-- | Evaluate a case to WHNF.
whnfCase :: Exp -> Id -> Typ -> [Alt] -> Eval WHNF
whnfCase e v _ty alts = do
  whnf <- whnfExp e
  choice <- patternMatch whnf alts
  whnfExp (betaSubstitute v e choice)

-- | Evaluate a let expression to WHNF.  Simply evaluate the body,
-- with the let bindings in scope.  This is non-strict, but not
-- lazy. We leave open the opportunity for laziness in the 'LetWHNF'
-- constructor that could be updated with evaluated variables.
whnfLet :: Bind -> Exp -> Eval WHNF
whnfLet bind e =
  local
    (\env -> env {envLets = insertBind bind (envLets env)})
    (do whnf <- whnfExp e
        pure (LetWHNF bind whnf))


-- | Create a WHNF value from a literal.
whnfLit :: Lit -> Eval WHNF
whnfLit =
  \case
    Char ch -> pure (PrimWHNF (CharPrim ch))
    Str bs ->
      liftIO
        (do Ptr addr <-
              S.useAsCStringLen
                bs
                (\(from, len) -> do
                   to <- callocBytes (len + 1)
                   S.memcpy to (coerce from) len
                   pure to)
            pure (PrimWHNF (AddrPrim (Addr addr))))
    NullAddr -> pure (PrimWHNF (AddrPrim (Addr nullAddr#)))
    Int i -> pure (PrimWHNF (IntPrim (fromIntegral i)))
    Int64 i -> pure (PrimWHNF (IntPrim (fromIntegral i)))
    Word i -> pure (PrimWHNF (WordPrim (fromIntegral i)))
    Word64 i -> pure (PrimWHNF (WordPrim (fromIntegral i)))
    Float i -> pure (PrimWHNF (FloatPrim (fromRational i)))
    Double i -> pure (PrimWHNF (DoublePrim (fromRational i)))
    Label -> pure LabelWHNF
    Integer i -> pure (IntegerWHNF i)

-- | Resolve a locally let identifier, a global identifier, to its expression.
whnfId :: Id -> Eval WHNF
whnfId i@(Id bs _ cat) =
  case cat of
    ClassCat -> pure (ConWHNF i [])
    DataCat -> pure (ConWHNF i [])
    ValCat
      | bs == "ghc-prim:GHC.Tuple.()" -> pure (ConWHNF i []) -- FIXME: Tuple, move this to GHC patch
    ValCat -> do
      methodRef <- asks envMethods
      methods <- liftIO (readIORef methodRef)
      case M.lookup (idStableName i) methods of
        Just index -> pure (MethodWHNF i index)
        Nothing -> do
          lets <- asks envLets
          case M.lookup i lets of
            Just e -> whnfExp e
            Nothing -> do
              globalRef <- asks envGlobals
              globals <- liftIO (readIORef globalRef)
              case M.lookup (idStableName i) globals of
                Just e -> do
                  depth <- asks envDepth
                  let indent = replicate (fromIntegral depth) ' '
                      out v = when True (liftIO (S8.putStrLn (S8.pack (indent ++ v))))
                  out
                    ("Resolved: " ++
                     show e ++ "\n" ++
                     L8.unpack
                       (L.toLazyByteString (pretty i <> " = " <> pretty e)))
                  whnfExp e
                Nothing ->
                  case M.lookup bs primops of
                    Just op -> pure (OpWHNF op [])
                    Nothing -> throw (NotInScope i)

-- | Replace all instances of @x@ with @replacement@, avoiding name capture.
betaSubstitute :: Id -> Exp -> Exp -> Exp
betaSubstitute x replacement = go
  where
    go =
      \case
        VarE y ->
          if x == y
            then replacement
            else VarE y
        e@LitE {} -> e
        AppE f a -> AppE (go f) (go a)
        LamE t y e ->
          if y == x
            then LamE t y e
            else LamE t y (go e)
        CaseE e y ty alts ->
          CaseE
            (go e)
            y
            ty
            (if y == x
               then alts
               else map (\(Alt con vars alt_e) -> Alt con vars (go alt_e)) alts)
        e@TypE {} -> e
        CoercionE -> CoercionE
        orig@(LetE bind let_e) ->
          case bind of
            NonRec y nonrec_e ->
              if y == x
                then LetE (NonRec y (go nonrec_e)) let_e
                else LetE (NonRec y (go nonrec_e)) (go let_e)
            Rec binds ->
              if any (\(y, _) -> y == x) binds
                then orig
                else LetE
                       (Rec (map (\(v, bind_e) -> (v, go bind_e)) binds))
                       (go let_e)
        CastE e -> CastE (go e)
        TickE e -> TickE (go e)

-- | Insert a binding into the let-local scope.
insertBind :: Bind -> Map Id Exp -> Map Id Exp
insertBind (NonRec k v) = M.insert k v
insertBind (Rec pairs) = \m0 -> foldl (\m (k, v) -> M.insert k v m) m0 pairs

-- | See whether an alt matches against a WHNF.
patternMatch :: WHNF -> [Alt] -> Eval Exp
patternMatch whnf alts =
  case whnf of
    ConWHNF (Id bs _ _) args ->
      case find
             ((\case
                 DataAlt (DataCon (Id bs' _ _) _) -> bs == bs'
                 _ -> False) .
              altCon)
             alts of
        Just alt ->
          pure
            (foldl'
               (\e (v, arg) -> betaSubstitute v arg e)
               (altExp alt)
               (zip (altVars alt) args))
        Nothing -> defaulting
    PrimWHNF prim ->
      case find
             ((\case
                 LitAlt lit -> litMatch lit prim
                 _ -> False) .
              altCon)
             alts of
        Just alt -> pure (altExp alt)
        Nothing -> defaulting
    IntegerWHNF i ->
      case find
             ((\case
                 LitAlt (Integer j) -> i == j
                 _ -> False) .
              altCon)
             alts of
        Nothing -> defaulting
        Just alt -> pure (altExp alt)
    _ -> failed
  where
    defaulting =
      case alts of
        alt@(Alt {altCon = DEFAULT}):_ -> pure (altExp alt)
        _ -> failed
    failed = throw (FailedPatternMatch whnf alts)

-- | Match a literal against a primitive value. Only numbers and char
-- are supported. Floating point comparison is not allowed here,
-- according to GHC.
litMatch :: Lit -> Prim -> Bool
litMatch l p =
  case (l, p) of
    (Char x, CharPrim y) -> x == y
    (Int x, IntPrim y) -> fromIntegral x == y
    (Int64 x, IntPrim y) -> fromIntegral x == y
    (Word x, WordPrim y) -> fromIntegral x == y
    (Word64 x, WordPrim y) -> fromIntegral x == y
    _ -> False

-- | Primitive operators.
primops :: Map ByteString Op
primops =
  M.fromList
    (map
       (opName &&& id)
       [ Op {opArity = 2, opName = "ghc-prim:GHC.Prim.tagToEnum#"}
       , Op {opArity = 2, opName = "ghc-prim:GHC.Prim.-#"}
       , Op {opArity = 2, opName = "ghc-prim:GHC.Prim.+#"}
       , Op {opArity = 2, opName = "ghc-prim:GHC.Prim.<#"}
       ])
