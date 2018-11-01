{-# LANGUAGE LambdaCase #-}
-- |

module Prana.Print where

import           CoreSyn
import           Data.List
import qualified GHC
import qualified Id as GHC
import qualified Literal as GHC
import qualified Name as GHC

showBind :: Bool -> CoreSyn.Bind GHC.Var -> String
showBind ps =
  \case
    CoreSyn.NonRec var expr ->
      parens ps ("NonRec " ++ showVar True var ++ " " ++ showExpr True expr)
    CoreSyn.Rec exprs ->
      parens
        ps
        ("Rec [" ++
         intercalate
           ","
           (map
              (\(x, y) ->
                 "(" ++ showVar False x ++ ", " ++ showExpr False y ++ ")")
              exprs) ++
         "]")

showVar :: Bool -> GHC.Var -> String
showVar ps v = parens ps ("Var " ++ show (GHC.nameStableString (GHC.getName v)))

showId :: Bool -> GHC.Id -> String
showId ps v = parens ps ("Id " ++ show (GHC.nameStableString (GHC.getName v)))

showExpr :: Bool -> CoreSyn.Expr GHC.Var -> String
showExpr ps =
  \case
    CoreSyn.Var vid -> parens ps ("Var " ++ showId ps vid)
    CoreSyn.Lit literal -> parens ps ("Lit " ++ showLiteral True literal)
    CoreSyn.App f x ->
      parens ps ("App " ++ showExpr True f ++ " " ++ showExpr True x)
    CoreSyn.Lam var body -> "Lam"
    CoreSyn.Let bind expr -> "Let"
    CoreSyn.Case expr var typ alts -> "Case"
    CoreSyn.Cast expr coercion -> "Cast"
    CoreSyn.Tick tickishVar expr -> "Tick"
    CoreSyn.Type typ -> "Type"
    CoreSyn.Coercion coercion -> "Coercion"

showLiteral :: Bool -> GHC.Literal -> String
showLiteral ps =
  \case
    GHC.MachChar ch -> parens ps ("MachChar " ++ show ch)
    GHC.MachStr str -> parens ps ("MachStr " ++ show str)
    GHC.MachNullAddr -> "MachNullAddr"
    GHC.MachInt i -> parens ps ("MachInt " ++ show i)
    GHC.MachInt64 i -> parens ps ("MachInt64 " ++ show i)
    GHC.MachWord i -> parens ps ("MachWord " ++ show i)
    GHC.MachWord64 i -> parens ps ("MachWord64 " ++ show i)
    GHC.MachFloat i -> parens ps ("MachFloat " ++ show i)
    GHC.MachDouble i -> parens ps ("MachDouble " ++ show i)
    GHC.MachLabel fastString mint functionOrData -> parens ps ("MachLabel ")
    GHC.LitInteger i typ -> parens ps ("LitInteger " ++ show i ++ " " ++ showType typ)

showType :: GHC.Type -> String
showType _ = "Type"

parens :: Bool -> [Char] -> [Char]
parens True x = "(" ++ x ++ ")"
parens _    x = x
