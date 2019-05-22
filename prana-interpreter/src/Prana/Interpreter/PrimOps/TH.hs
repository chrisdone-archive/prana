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
    , optionsEvalSmallMutableArray :: !Name
    , optionsBoxSmallMutableArray :: !Name
    , optionsEvalMutableByteArray :: !Name
    , optionsBoxMutableByteArray :: !Name
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
           (listP (map varP patNames))
           (normalB
              (doE
                 (concat
                    [ bindings
                    , [ bindS
                          (bangP (varP getResultName))
                          (if any (== stateS) (primArgTys ty)
                             then appE
                                    (conE 'IO)
                                    (lamE
                                       [varP stateName]
                                       (caseE
                                          (foldl'
                                             appE
                                             (varE (mkName primName))
                                             (map varE resultNames ++
                                              [varE stateName]))
                                          [ match
                                              (case tupleSlotNames of
                                                 [slot] -> varP slot
                                                 _ ->
                                                   unboxedTupP
                                                     (map varP tupleSlotNames))
                                              (normalB
                                                 (unboxedTupE
                                                    [ varE (head tupleSlotNames)
                                                    , lamE
                                                        [wildP]
                                                        (unboxedTupE
                                                           (map
                                                              varE
                                                              tupleSlotNames))
                                                    ]))
                                              []
                                          ]))
                             else appE
                                    (varE 'pure)
                                    (lamE
                                       [wildP]
                                       (foldl'
                                          appE
                                          (varE (mkName primName))
                                          (map varE resultNames))))
                      , letS
                          [ valD
                              (if primReturnTy ty == Just stateS
                                 then wildP
                                 else varP resultName)
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
                 (appE
                    (varE 'concat)
                    (listE
                       [ stringE
                           ("Invalid arguments to primop " ++ show primName ++ ": ")
                       , appE (varE 'show) (varE (optionsArgs options))
                       ]))))
           []
       ])
  where
    stateS = TyApp (TyCon ("State#")) [TyVar "s"]
    stateName = mkName "s"
    getResultName = mkName "get_result"
    resultName = mkName "result"
    patNames = zipWith argName [0 ..] (primArgTys ty)
    argNames = zipWith argName [0 ..] (filter (/= stateS) (primArgTys ty))
    resultNames =
      zipWith mkresultName [0 ..] (filter (/= stateS) (primArgTys ty))
    tupleSlotNames =
      zipWith
        mktupleSlotName
        [0 ..]
        (case primReturnTy ty of
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
wrapResult :: Options -> [Char] -> Name -> Ty -> Either [Char] [Q Stmt]
wrapResult options primName resultName ty =
  case primReturnTy ty of
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
    Just (TyApp (TyCon ("State#")) [TyVar "s"]) ->
      pure [noBindS [|pure StateWhnf|]]
    Just retTy -> Left (primName ++ ": Unknown return type " ++ show retTy)
    Nothing -> Left "No return type!"

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
    TyApp (TyCon ("State#")) [TyVar "s"] ->
      pure (noBindS (appE (varE 'pure) (tupE [])))
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
wrapUnboxedTuple :: Options -> [Ty] -> Name -> String -> Either String [StmtQ]
wrapUnboxedTuple options slotTypes resultName primName = do
  binds <-
    sequence
      (zipWith
         (\i slotTy -> do
            boxer <-
              case slotTy of
                (TyApp (TyCon "Int#") []) -> pure (varE (optionsBoxInt options))
                (TyApp (TyCon "Word#") []) -> pure (varE (optionsBoxWord options))
                (TyApp (TyCon "Char#") []) -> pure (varE (optionsBoxChar options))
                (TyApp (TyCon "Double#") []) -> pure (varE (optionsBoxDouble options))
                (TyApp (TyCon "Float#") []) -> pure (varE (optionsBoxFloat options))
                (TyApp (TyCon "Addr#") []) -> pure (varE (optionsBoxAddr options))
                TyApp (TyCon ("MutableArray#")) [TyVar "s", TyVar "a"] ->
                  pure (varE (optionsBoxMutableArray options))
                TyApp (TyCon ("MutableByteArray#")) [TyVar "s"] ->
                  pure (varE (optionsBoxMutableByteArray options))
                TyApp (TyCon ("SmallMutableArray#")) [TyVar "s", TyVar "a"] ->
                  pure (varE (optionsBoxSmallMutableArray options))
                TyApp (TyCon ("Array#")) [TyVar "a"] ->
                  pure (varE (optionsBoxArray options))
                TyVar "a" -> pure (varE 'pure)
                TyApp (TyCon ("State#")) [TyVar "s"] -> pure (varE 'boxState)
                _ ->
                  Left
                    ("Invalid type for unboxed tuple slot: " ++
                     show slotTy ++ " in " ++ primName)
            pure
              (bindS
                 (varP (mkSlotNameBoxed i))
                 (appE boxer (varE (mkSlotName i)))))
         [0 :: Int ..]
         slotTypes)
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
                        [ binds
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
