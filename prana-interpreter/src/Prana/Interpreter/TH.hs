{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Prana.Interpreter.TH
  (Prim (..)
  ,PrimTy(..)
  ,primOpAlt)
  where

import Data.List
import GHC.Exts
import Language.Haskell.TH
import Prana.Interpreter.Types (Whnf(..))
import Prana.Types (Lit(..))

data Prim =
  Prim
    { primArgTys :: [PrimTy]
    , primReturnTy :: PrimTy
    , primName :: Name
    }
data PrimTy =
  PrimTyI#
  deriving (Show)

{-

Example:

case args of
  [arg1, arg2] -> do
    i <- evalIntArg index globals locals arg1
    i2 <- evalIntArg index globals locals arg2
    -- print (show i ++ " +# " ++ show i2)
    let !r = i - i2
    pure (LitWhnf (IntLit r))
  _ -> error ("Invalid arguments to IntSubOp: " ++ show args)

-}

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
                          PrimTyI# ->
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
                       PrimTyI# ->
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
