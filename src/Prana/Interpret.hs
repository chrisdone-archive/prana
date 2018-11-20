{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- |

module Prana.Interpret where

import           Control.Concurrent
import           Control.Exception (Exception, throw)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Typeable
import           Foreign.Marshal
import           GHC.Base
import           GHC.Exts
import           Prana.Types

-- | An environment to evaluate expressions in.
data Env = Env
  { envGlobals :: !(IORef (Map Id Exp))
  , envLets :: !(Map Var Exp)
  }

-- | Evaluation computation.
newtype Eval a = Eval
  { runEval :: ReaderT Env IO a
  } deriving (MonadIO, Monad, Applicative, Functor, MonadReader Env)

-- | A interpreter error in the interpreter.
data InterpreterError
  = TypeError TypeError
  | NotInScope Id
  deriving (Show, Typeable)
instance Exception InterpreterError

-- | A type error in the interpreter.
data TypeError =
  NotAFunction WHNF
  deriving (Show, Typeable)

-- | An expression evaluated to weak head normal form.
data WHNF
  = OpWHNF !Id
  | PrimWHNF !Prim
  | IntegerWHNF !Integer
  | ConWHNF !Id ![Exp]
  | LamWHNF !Var !Exp
  | LabelWHNF
  | CoercionWHNF
  | TypWHNF !Typ
  | LetWHNF !Bind !WHNF
  deriving (Show)

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

-- | Some address from GHC.Prim.
data Addr = Addr !Addr#
instance Show Addr where
  show (Addr a) = show (I# (addr2Int# a))

-- | Interpret the expression.
whnfExp :: Exp -> Eval WHNF
whnfExp =
  \case
    -- No-op, lambdas are self-evaluating:
    LamE i e -> pure (LamWHNF i e)
    -- No-op, types are self-evaluating:
    TypE ty -> pure (TypWHNF ty)
    -- No-op, coerciones are self-evaluating:
    CoercionE -> pure CoercionWHNF
    -- Skip over ticks:
    TickE e -> whnfExp e
    -- Skip over casts:
    CastE e -> whnfExp e
    -- Lookup globals, primitives and lets:
    VarE l -> resolveVar l >>= whnfExp
    -- Evaluate the body of a let, put the binding in scope:
    LetE bind e -> whnfLet bind e
    -- Produce a primitive/runtime value from the literal:
    LitE l -> litWHNF l
    AppE f arg -> do
      result <- whnfExp f
      case result of
        LamWHNF v body -> error "TODO: beta-substitute!"
        OpWHNF i -> error "TODO: force the args, run the primop!"
        ConWHNF i args -> pure (ConWHNF i (args ++ [arg]))
        _ -> throw (TypeError (NotAFunction result))
    -- Case analysis.
    CaseE e v ty alts ->
      do result <- whnfExp e
         undefined -- TODO: perform pattern match.

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
litWHNF :: Lit -> Eval WHNF
litWHNF =
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

-- | Insert a binding into the let-local scope.
insertBind :: Bind -> Map Var Exp -> Map Var Exp
insertBind (NonRec k v) = M.insert k v
insertBind (Rec pairs) = \m0 -> foldl (\m (k, v) -> M.insert k v m) m0 pairs

-- | Resolve a locally let identifier, a global identifier, to its expression.
resolveVar :: Id -> Eval Exp
resolveVar (Id u) = do
  lets <- asks envLets
  case M.lookup (Var u) lets of
    Just e -> pure e
    Nothing -> do
      globalRef <- asks envGlobals
      globals <- liftIO (readIORef globalRef)
      case M.lookup (Id u) globals of
        Just e -> pure e
        Nothing -> throw (NotInScope (Id u))
