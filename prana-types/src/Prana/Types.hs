-- |

module Prana.Types where

data Id

data Exp e
  = Var Id
  | Lit Lit
  | App e e
  | Lem Id e
  | Let (Bind e) e
  | Case e Id Type [Alt e]

data Bind e

data Alt e

data Type

data Lit
