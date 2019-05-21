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

import Control.Monad
import Data.List
import Data.Maybe
import GHC.Exts
import Language.Haskell.TH
import Prana.Interpreter.Types
import Prana.PrimOp (ty, name, Entry(..), parsePrimops, Ty(..), TyCon(..))
import Prana.Types (DataConId(..), Lit(..), Arg(..))

data Options =
  Options
    { optionsOp :: !Name
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
    , optionsEvalSomeVarId :: !Name
    , optionsManualImplementations :: ![(Name, Name)]
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
              case derivePrimOpAlt options name ty of
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

derivePrimOpAlt :: Options -> String -> Ty -> Either String (Q Exp)
derivePrimOpAlt options primName ty = do
  bindings <-
    mapM
      (unwrapArg primName options)
      (zip resultNames (zip argNames (primArgTys ty)))
  retStatments <- wrapResult options primName resultName ty
  pure
    (caseE
       (varE (optionsArgs options))
       [ match
           (listP (map varP argNames))
           (normalB
              (doE
                 (concat
                    [ bindings
                    , [ letS
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
    resultName = mkName "result"
    argNames = zipWith argName [0 ..] (primArgTys ty)
    resultNames = zipWith mkresultName [0 ..] (primArgTys ty)
    argName :: Int -> a -> Name
    argName i _ = mkName ("wrapped_arg_" ++ show i)
    mkresultName :: Int -> a -> Name
    mkresultName i _ = mkName ("arg_unwrapped_" ++ show i)

-- | Wrap up a primop's result back in guest representation.
wrapResult :: Options -> [Char] -> Name -> Ty -> Either [Char] [Q Stmt]
wrapResult options primName resultName ty =
  case primReturnTy ty of
    Just (TyApp (TyCon "Int#") []) -> wrapLit resultName 'IntLit 'I#
    Just (TyApp (TyCon "Char#") []) -> wrapLit resultName 'CharLit 'C#
    Just (TyApp (TyCon "Word#") []) -> wrapLit resultName 'WordLit 'W#
    Just (TyApp (TyCon "Addr#") []) -> wrapWhnf resultName 'AddrWhnf (varE 'id) 'Ptr
    Just (TyApp (TyCon "Double#") []) -> wrapLit resultName 'DoubleLit 'D#
    Just (TyApp (TyCon "Float#") []) -> wrapLit resultName 'FloatLit 'F#
    Just (TyUTup slotTypes) -> wrapUnboxedTuple options slotTypes resultName primName
    Just retTy -> Left (primName ++ ": Unknown return type " ++ show retTy)
    Nothing -> Left (primName ++ ": Couldn't find return type of " ++ show ty)

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


primArgTys :: Ty -> [Ty]
primArgTys =
  \case
    TyF arg rest -> arg : primArgTys rest
    _ -> []

primReturnTy :: Ty -> Maybe Ty
primReturnTy =
  \case
    TyF _ rest -> primReturnTy rest
    ty -> Just ty

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
