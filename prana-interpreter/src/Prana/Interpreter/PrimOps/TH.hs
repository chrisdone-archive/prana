{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
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

import Data.List
import Data.Maybe
import GHC.Exts
import Language.Haskell.TH
import Prana.Interpreter.Types
import Prana.PrimOp (ty, name, Entry(..), parsePrimops, Ty(..), TyCon(..))
import Prana.Types (DataConId(..), Lit(..))

data Options =
  Options
    { optionsOp :: !Name
    , optionsArgs :: !Name
    , optionsEvalInt :: !Name
    , optionsBoxInt :: !Name
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
                Left {} -> do
                  -- reportWarning (cons ++ ": " ++ e)
                  pure Nothing
                Right expr ->
                  pure (Just (match (conP (mkName cons) []) (normalB expr) []))
            _ -> pure Nothing)
         entries)
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
    def =
      match
        wildP
        (normalB
           (appE (varE 'error) (appE (varE 'show) (varE (optionsOp options)))))
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

wrapResult :: Options -> [Char] -> Name -> Ty -> Either [Char] [Q Stmt]
wrapResult options primName resultName ty =
  case primReturnTy ty of
    Just (TyApp (TyCon "Int#") []) ->
      pure
        [ noBindS
            (appE
               (varE 'pure)
               (appE
                  (conE 'LitWhnf)
                  (appE (conE 'IntLit) (appE (conE 'I#) (varE resultName)))))
        ]
    Just (TyUTup slotTypes) ->
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
                                (\i _ty ->
                                   bindS
                                     (varP (mkSlotName i))
                                     (appE
                                        (varE (optionsBoxInt options))
                                        (varE (mkSlotName i))))
                                [0 :: Int ..]
                                slotTypes
                            , [ noBindS
                                  (appE
                                     (varE 'pure)
                                     (appE (appE
                                              (conE 'ConWhnf)
                                              (appE
                                                 (conE 'UnboxedTupleConId)
                                                 (litE
                                                    (IntegerL
                                                       (fromIntegral
                                                          (length slotTypes))))))
                                           (listE
                                              (zipWith
                                                 (\i _ -> varE (mkSlotName i))
                                                 [0 :: Int ..]
                                                 slotTypes))))
                              ]
                            ])))
                   []
               ])
        ]
      where mkSlotName i = mkName ("slot_" ++ show i)
    Just retTy -> Left (primName ++ ": Unknown return type " ++ show retTy)
    Nothing -> Left (primName ++ ": Couldn't find return type of " ++ show ty)

unwrapArg :: [Char] -> Options -> (Name, (Name, Ty)) -> Either [Char] StmtQ
unwrapArg primName options =
  \(result, (arg, argTy)) ->
    case argTy of
      TyApp (TyCon "Int#") [] ->
        pure
          (bindS
             (conP 'I# [varP result])
             (appE
                (appE
                   (varE (optionsEvalInt options))
                   (varE (optionsEvalSomeVarId options)))
                (varE arg)))
      _ -> Left (primName ++ ": Unknown arg type: " ++ show argTy)

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
