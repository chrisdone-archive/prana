{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Prana.Interpreter.TH
  (Prim (..)
  ,PrimTy(..)
  ,primOpAlt
  ,op)
  where

import Data.Bifunctor
import Data.List
import GHC.Exts
import Language.Haskell.TH hiding (Prim)
import Prana.Interpreter.Types (Whnf(..))
import Prana.Types (Lit(..))

data Prim =
  Prim
    { primArgTys :: [PrimTy]
    , primReturnTy :: PrimTy
    , primName :: Name
    }
data PrimTy =
  PrimTyInt#
  deriving (Show)

primOpAlt :: Prim -> Q Exp
primOpAlt p =
  caseE
    (varE (mkName "args"))
    [ match
        (listP (map varP argNames))
        (normalB
           (doE
              (concat
                 [ map
                     (\(result, (arg, ty)) ->
                        case ty of
                          PrimTyInt# ->
                            bindS
                              (conP 'I# [varP result])
                              [|evalIntArg index globals locals $(varE arg)|])
                     (zip resultNames (zip argNames (primArgTys p)))
                 , [ letS
                       [ valD
                           (bangP (varP resultName))
                           (normalB
                              (foldl'
                                 appE
                                 (varE (primName p))
                                 (map varE resultNames)))
                           []
                       ]
                   , case primReturnTy p of
                       PrimTyInt# ->
                         noBindS
                           (appE
                              (varE 'pure)
                              (appE
                                 (conE 'LitWhnf)
                                 (appE
                                    (conE 'IntLit)
                                    (appE (conE 'I#) (varE resultName)))))
                   ]
                 ])))
        []
    , match
        wildP
        (normalB
           (appE
              (varE 'error)
              (stringE ("Invalid arguments to primop: " ++ show (primName p)))))
        []
    ]
  where
    resultName = mkName "result"
    argNames = zipWith argName [0 ..] (primArgTys p)
    resultNames = zipWith mkresultName [0 ..] (primArgTys p)
    argName :: Int -> PrimTy -> Name
    argName i _ = mkName ("arg_" ++ show i)
    mkresultName :: Int -> PrimTy -> Name
    mkresultName i ty = mkName ("result_" ++ show i ++ "_" ++ show ty)

op :: Q Exp -> Q Exp
op q = do
  e <- q
  case e of
    SigE (VarE opName) typ ->
      let (args, ret) = collect typ
       in primOpAlt
            Prim {primName = opName, primArgTys = args, primReturnTy = ret}
    _ -> error "Form expected: name :: ty"
  where
    collect :: Type -> ([PrimTy], PrimTy)
    collect (AppT (AppT ArrowT arg) result) = first (toPrim arg :) (collect result)
    collect (ConT ty) = ([],toPrim (ConT ty))
    collect (InfixT arg1 name arg2) = collect (AppT (AppT (ConT name) arg1) arg2)
    collect _ = error "Malformed!"
    toPrim (ConT ty) =
      if ty == ''Int#
        then PrimTyInt#
        else error ("Unknown type: " ++ show ty)
    toPrim t = error ("Expected type constructor here, but got " ++ show t)
