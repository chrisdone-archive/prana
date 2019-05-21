{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |

module Prana.Interpreter.PrimOps.TH where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Primitive
import           GHC.Exts
import           GHC.Types (IO(..))
import           Language.Haskell.TH
import           Prana.Interpreter.Boxing
import           Prana.Interpreter.Types
import           Prana.PrimOp (ty, name, Entry(..), parsePrimops, Ty(..), TyCon(..))
import           Prana.Types (DataConId(..), Lit(..), Arg(..))

data Options =
  Options
    { optionsOp :: !Name
    , optionsLocals :: !Name
    , optionsArgs :: !Name
    , optionsEvalInt :: !Name
    , optionsBoxInt :: !Name
    , optionsEvalChar :: !Name
    , optionsBoxChar :: !Name
    , optionsEvalDouble :: !Name
    , optionsBoxDouble :: !Name
    , optionsEvalFloat :: !Name
    , optionsBoxFloat :: !Name
    , optionsEvalWord :: !Name
    , optionsBoxWord :: !Name
    , optionsEvalAddr :: !Name
    , optionsBoxAddr :: !Name
    , optionsIndex :: !Name
    , optionsType :: !Name
    , optionsEvalArray :: !Name
    , optionsBoxArray :: !Name
    , optionsEvalMutableArray :: !Name
    , optionsBoxMutableArray :: !Name
    , optionsEvalSomeVarId :: !Name
    , optionsManualImplementations :: ![(Name, Name)]
    , optionsEvalBox :: !Name
    }

derivePrimOpsCase :: Options -> Q Exp
derivePrimOpsCase options = do
  entries <- runIO parsePrimops
  derived <-
    fmap
      catMaybes
      (mapM
         (\case
            PrimOpSpec {cons, name, ty} -> do
              case derivePrimOpAlt options name (tyBehavior ty) of
                Left e -> do
                  when True (reportWarning (cons ++ ": " ++ e))
                  pure Nothing
                Right expr ->
                  pure (Just (match (conP (mkName cons) []) (normalB expr) []))
            _ -> pure Nothing)
         (ignoreSections id entries))
  let manuals =
        map
          (\(conName, funName) ->
             match
               (conP conName [])
               (normalB
                  (appE
                     (appE
                        (appE
                           (appE (varE funName) (varE (optionsIndex options)))
                           (varE (optionsType options)))
                        (varE (optionsEvalSomeVarId options)))
                     (varE (optionsArgs options))))
               [])
          (optionsManualImplementations options)
  caseE (varE (optionsOp options)) (derived <> manuals <> [def])
  where
    ignoreSections keep entries =
      case break
             (\case
                Section {} -> True
                _ -> False)
             entries of
        (before, Section {title}:after)
          | elem
             title
             [ "Bytecode operations"
             , "Exceptions"
             , "STM-accessible Mutable Variables"
             , "Concurrency primitives"
             , "Weak pointers"
             , "Stable pointers and names"
             , "Compact normal form"
             , "Parallelism"
             , "Prefetch"
             , "Etc"
             , "Synchronized Mutable Variables"
             ] -> ignoreSections (const []) after
          | otherwise -> keep before <> ignoreSections id after
        (before, after) -> keep before <> after
    def =
      match
        wildP
        (normalB
           (appE
              (varE 'error)
              (appE
                 (appE (varE '(++)) (stringE "Unimplemented primop: "))
                 (appE (varE 'show) (varE (optionsOp options))))))
        []

derivePrimOpAlt :: Options -> String -> Behavior -> Either String (Q Exp)
derivePrimOpAlt options primName ty = do
  bindings <-
    mapM
      (unwrapArg primName options)
      (zip resultNames (zip argNames (signatureArgs (behaviorSig ty))))
  retStatments <- wrapResult options primName resultName (behaviorSig ty)
  pure
    (caseE
       (varE (optionsArgs options))
       [ match
           (listP (map varP argNames))
           (normalB
              (doE
                 (concat
                    [ bindings
                    , case ty of
                        Pure {} ->
                          [ letS
                              [ valD
                                  (bangP (varP resultName))
                                  (normalB
                                     (foldl'
                                        appE
                                        (varE (mkName primName))
                                        (map varE resultNames)))
                                  []
                              ]
                          ]
                        Monadic {} ->
                          [ bindS
                              (varP getResultName)
                              (appE
                                 (conE 'IO)
                                 (lamE
                                    [varP stateName]
                                    (caseE
                                       (appE
                                          (foldl'
                                             appE
                                             (varE (mkName primName))
                                             (map varE resultNames))
                                          (varE stateName))
                                       [ match
                                           (if null tupleSlotNames
                                               then varP newStateName
                                               else unboxedTupP
                                                      (varP newStateName :
                                                       map varP tupleSlotNames))
                                           (normalB
                                              (unboxedTupE
                                                 [ varE newStateName
                                                 , lamE
                                                     [wildP]
                                                     (case tupleSlotNames of
                                                        [name] -> varE name
                                                        [] -> tupE []
                                                        _ -> unboxedTupE
                                                                 (map varE tupleSlotNames))
                                                 ]))
                                           []
                                       ])))
                          , letS
                              [ valD
                                  (if null tupleSlotNames
                                      then wildP
                                      else  bangP (varP resultName))
                                  (normalB (appE (varE getResultName) (tupE [])))
                                  []
                              ]
                          ]
                    , retStatments
                    ])))
           []
       , match
           wildP
           (normalB
              (appE
                 (varE 'error)
                 (stringE ("Invalid arguments to primop: " ++ show primName))))
           []
       ])
  where
    stateName = mkName "s"
    newStateName = mkName "s'"
    getResultName = mkName "get_result"
    resultName = mkName "result"
    argNames = zipWith argName [0 ..] (signatureArgs (behaviorSig ty))
    resultNames = zipWith mkresultName [0 ..] (signatureArgs (behaviorSig ty))
    tupleSlotNames =
      zipWith mktupleSlotName [0 ..] (case signatureReturn (behaviorSig ty) of
                                        Just (TyUTup tys) -> tys
                                        Just t -> [t]
                                        _ -> [])
    argName :: Int -> a -> Name
    argName i _ = mkName ("wrapped_arg_" ++ show i)
    mkresultName :: Int -> a -> Name
    mkresultName i _ = mkName ("arg_unwrapped_" ++ show i)
    mktupleSlotName :: Int -> a -> Name
    mktupleSlotName i _ = mkName ("arg_tmp_" ++ show i)

-- | Wrap up a primop's result back in guest representation.
wrapResult :: Options -> [Char] -> Name -> Signature -> Either [Char] [Q Stmt]
wrapResult options primName resultName ty =
  case signatureReturn ty of
    Just (TyApp (TyCon "Int#") []) -> wrapLit resultName 'IntLit 'I#
    Just (TyApp (TyCon "Char#") []) -> wrapLit resultName 'CharLit 'C#
    Just (TyApp (TyCon "Word#") []) -> wrapLit resultName 'WordLit 'W#
    Just (TyApp (TyCon "Addr#") []) ->
      wrapWhnf resultName 'AddrWhnf (varE 'id) 'Ptr
    Just (TyApp (TyCon "Double#") []) -> wrapLit resultName 'DoubleLit 'D#
    Just (TyApp (TyCon "Float#") []) -> wrapLit resultName 'FloatLit 'F#
    Just (TyUTup slotTypes) ->
      wrapUnboxedTuple options slotTypes resultName primName
    Just (TyApp (TyCon ("Array#")) [TyVar "a"]) ->
      wrapWhnf resultName 'ArrayWhnf (varE 'id) 'Array
    Just (TyApp (TyCon ("MutableArray#")) [TyVar "s", TyVar "a"]) ->
      wrapWhnf
        resultName
        'MutableArrayWhnf
        (conE 'MutableRealWorldArray)
        'MutableArray
    Just (TyVar "a") ->
      pure [noBindS (appE (varE (optionsEvalBox options)) (varE resultName))]
    Just retTy -> Left (primName ++ ": Unknown return type " ++ show retTy)
    Nothing -> pure [noBindS [|pure EmptyWhnf|]]

-- | Wrap up a value in a literal WHNF.
wrapLit :: Applicative f => Name -> Name -> Name -> f [StmtQ]
wrapLit resultName litCon valCon = wrapWhnf resultName 'LitWhnf (conE litCon) valCon

-- | Wrap up a value in a WHNF.
wrapWhnf :: Applicative f => Name -> Name -> ExpQ -> Name -> f [StmtQ]
wrapWhnf resultName whnfCon litCon valCon =
  pure
    [ noBindS
        (appE
           (varE 'pure)
           (appE
              (conE whnfCon)
              (appE litCon (appE (conE valCon) (varE resultName)))))
    ]

-- | Wrap up an unboxed tuple, boxing its arguments. That's our design decision.
-- TODO: Re-visit boxing unboxed arguments? Maybe only do polymorphic ones?
wrapUnboxedTuple :: Applicative f => Options -> [Ty] -> Name -> String -> f [StmtQ]
wrapUnboxedTuple options slotTypes resultName primName =
  pure
    [ noBindS
        (caseE
           (varE resultName)
           [ match
               (unboxedTupP
                  (zipWith
                     (\i _ty -> varP (mkSlotName i))
                     [0 :: Int ..]
                     slotTypes))
               (normalB
                  (doE
                     (concat
                        [ zipWith
                            (\i slotTy ->
                               bindS
                                 (varP (mkSlotNameBoxed i))
                                 (appE
                                    (case slotTy of
                                       (TyApp (TyCon "Int#") []) ->  varE (optionsBoxInt options)
                                       (TyApp (TyCon "Word#") []) ->  varE (optionsBoxWord options)
                                       (TyApp (TyCon "Char#") []) ->  varE (optionsBoxChar options)
                                       (TyApp (TyCon "Double#") []) ->  varE (optionsBoxDouble options)
                                       (TyApp (TyCon "Float#") []) ->  varE (optionsBoxFloat options)
                                       (TyApp (TyCon "Addr#") []) ->  varE (optionsBoxAddr options)
                                       TyVar "a" -> varE 'pure
                                       _ ->
                                         error
                                           ("Invalid type for unboxed tuple slot: " ++ show slotTy ++ " in " ++ primName))
                                    (varE (mkSlotName i))))
                            [0 :: Int ..]
                            slotTypes
                        , [ noBindS
                              (appE
                                 (varE 'pure)
                                 (appE
                                    (appE
                                       (conE 'ConWhnf)
                                       (appE
                                          (conE 'UnboxedTupleConId)
                                          (litE
                                             (IntegerL
                                                (fromIntegral (length slotTypes))))))
                                    (listE
                                       (zipWith
                                          (\i _ -> varE (mkSlotNameBoxed i))
                                          [0 :: Int ..]
                                          slotTypes))))
                          ]
                        ])))
               []
           ])
    ]
  where
    mkSlotName i = mkName ("slot_" ++ show i)
    mkSlotNameBoxed i = mkName ("slot_boxed_" ++ show i)

-- | Eval and unwrap an argument from our representation to primitive host Haskell.
unwrapArg :: [Char] -> Options -> (Name, (Name, Ty)) -> Either [Char] StmtQ
unwrapArg primName options (result, (arg, argTy)) =
  case argTy of
    TyApp (TyCon "Int#") [] -> unwrap 'I# optionsEvalInt
    TyApp (TyCon "Char#") [] -> unwrap 'C# optionsEvalChar
    TyApp (TyCon "Word#") [] -> unwrap 'W# optionsEvalWord
    TyApp (TyCon "Double#") [] -> unwrap 'D# optionsEvalDouble
    TyApp (TyCon "Float#") [] -> unwrap 'F# optionsEvalFloat
    TyApp (TyCon "Addr#") [] -> unwrap 'Ptr optionsEvalAddr
    TyVar "a" ->
      pure
        (bindS
           (varP result)
           (appE (appE (varE 'boxArg) (varE (optionsLocals options))) (varE arg)))
    TyApp (TyCon ("Array#")) [TyVar "a"] -> unwrap 'Array optionsEvalArray
    TyApp (TyCon ("MutableArray#")) [TyVar "s", TyVar "a"] ->
      unwrap 'MutableArray optionsEvalMutableArray
    _ -> Left (primName ++ ": Unknown arg type: " ++ show argTy)
  where
    unwrap hostCon evaler =
      pure
        (bindS
           (conP hostCon [varP result])
           (appE
              (appE
                 (varE (evaler options))
                 (varE (optionsEvalSomeVarId options)))
              (varE arg)))

-- | Given a constructor for WHNF, evaluate or unwrap that for use in
-- host haskell.
evalArgByType :: Name -> Name -> Q Exp
evalArgByType evalSomeVarId conName =
  [|\case
      LitArg $(conP conName [varP (mkName "i")]) -> pure i
      LitArg lit -> error ("Invalid lit rep: " ++ show lit)
      VarArg someVarId -> do
        whnf <- $(varE evalSomeVarId) someVarId
        case whnf of
          LitWhnf $(conP conName [varP (mkName "i")]) -> pure i
          LitWhnf lit -> error ("Invalid lit rep: " ++ show lit)
          _ -> error ("Unexpected whnf for evaluating a literal")|]


--------------------------------------------------------------------------------
-- Type deconstruction

data Signature =
  Signature
    { signatureArgs :: [Ty]
    , signatureReturn :: Maybe Ty
    }
  deriving (Show)

data Behavior
  = Monadic
      { behaviorSig :: !Signature
      }
  | Pure
      { behaviorSig :: !Signature
      }
  deriving (Show)

tyBehavior :: Ty -> Behavior
tyBehavior ty =
  case returnTy of
    Just (TyUTup (TyApp (TyCon ("State#")) [TyVar "s"]:rest)) ->
      Monadic
        Signature
          { signatureArgs = argTys
          , signatureReturn =
              if null rest
                then Nothing
                else case rest of
                       [rty] -> Just rty
                       _ -> Just (TyUTup rest)
          }
    Just (TyApp (TyCon ("State#")) [TyVar "s"]) ->
      Monadic Signature {signatureArgs = argTys, signatureReturn = Nothing}
    Nothing -> error ("No return type for type: " ++ show ty)
    _ -> Pure (Signature {signatureArgs = argTys, signatureReturn = returnTy})
  where
    argTys = filter ignoreState (primArgTys ty)
    returnTy = primReturnTy ty
    ignoreState (TyApp (TyCon ("State#")) [TyVar "s"]) = False
    ignoreState _ = True

primArgTys :: Ty -> [Ty]
primArgTys =
  \case
    TyF arg rest -> arg : primArgTys rest
    _ -> []

primReturnTy :: Ty -> Maybe Ty
primReturnTy (TyF _ rest) = Just (go rest)
  where
    go =
      \case
        TyF _ xs -> go xs
        ty -> ty
primReturnTy _ = Nothing

--------------------------------------------------------------------------------
-- How to extract unboxed values out of IO

-- readCharOffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Char##)
foo :: IO (() -> Char#)
foo =
  IO
    (\s ->
       case readCharOffAddr# nullAddr# 0# s of
         (# s', v #) -> (# s', \_ -> v #))

-- threadStatus# :: ThreadId# -> State# RealWorld -> (#State# RealWorld, Int#, Int#, Int##)
bar :: IO (() -> (# Int#, Int#, Int# #))
bar =
  IO
    (\s ->
       case threadStatus# undefined s of
         (# s', x, y, z #) -> (# s', \() -> (# x, y, z #) #))
