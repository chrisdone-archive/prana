{-# LANGUAGE LambdaCase #-}

-- |

module Prana.Pretty where

import           Data.List
import qualified Data.Map.Strict as M
import           Prana.Types

prettyRhs :: ReverseIndex -> Rhs -> String
prettyRhs index =
  \case
    RhsCon con -> node "RhsCon" [prettyCon index con]

prettyCon :: ReverseIndex -> Con -> [Char]
prettyCon index (Con dataConId args) =
  node
    "Con"
    [prettyDataConId index dataConId, prettyList (map (prettyArg index) args)]

prettyExpr :: ReverseIndex -> Expr -> String
prettyExpr index =
  \case
    AppExpr someVarId args ->
      node
        "AppExpr"
        [ prettySomeVarId index someVarId
        , prettyList (map (prettyArg index) args)
        ]
    ConAppExpr dataConId args _ty ->
      node
        "ConAppExpr"
        [ prettyDataConId index dataConId
        , prettyList (map (prettyArg index) args)
        ]
    OpAppExpr op args primType ->
      node
        "OpAppExpr"
        [show op, prettyList (map (prettyArg index) args), show primType]
    CaseExpr expr localVarId alts ->
      node
        "CaseExpr"
        [ prettyExpr index expr
        , prettyLocalVar index localVarId
        , prettyAlts index alts
        ]
    LetExpr binding expr -> node "LetExpr[TODO]" [prettyExpr index expr]
    LitExpr lit -> show lit

prettyAlts :: ReverseIndex -> Alts -> [Char]
prettyAlts index =
  \case
    PolymorphicAlt e -> node "PolymorphicAlt" [prettyExpr index e]
    DataAlts tyCon dataAlts mexpr ->
      node
        "DataAlts"
        [ prettyTyCon tyCon
        , prettyList (map prettyDataAlt dataAlts)
        , maybe "Nothing" (prettyExpr index) mexpr
        ]
    MultiValAlts i dataAlts mexpr ->
      node
        "MultiValAlts"
        [ show i
        , prettyList (map prettyDataAlt dataAlts)
        , maybe "Nothing" (prettyExpr index) mexpr
        ]
    PrimAlts primRep litAlts mexpr ->
      node
        "PrimAlts"
        [ show primRep
        , prettyList (map prettyLitAlt litAlts)
        , maybe "Nothing" (prettyExpr index) mexpr
        ]
  where
    prettyTyCon = show
    prettyLitAlt litAlt =
      node
        "LitAlt"
        [ show (litAltLit litAlt)
        , prettyList (map (prettyLocalVar index) (litAltBinders litAlt))
        , prettyExpr index (litAltExpr litAlt)
        ]
    prettyDataAlt dataAlt =
      node
        "DataAlt"
        [ prettyDataConId index (dataAltCon dataAlt)
        , prettyList (map (prettyLocalVar index) (dataAltBinders dataAlt))
        , prettyExpr index (dataAltExpr dataAlt)
        ]

prettyLocalVar :: ReverseIndex -> LocalVarId -> String
prettyLocalVar index localVarId =
  case M.lookup localVarId (reverseIndexLocals index) of
    Nothing -> error "Couldn't find name! BUG!"
    Just name -> show (displayName name)

prettySomeVarId :: ReverseIndex -> SomeVarId -> String
prettySomeVarId index =
  \case
    SomeLocalVarId localVarId -> prettyLocalVar index localVarId
    SomeGlobalVarId globalVarId ->
      (case M.lookup globalVarId (reverseIndexGlobals index) of
         Nothing -> error "Couldn't find name! BUG!"
         Just name -> show (displayName name))
    w@WiredInVal {} -> error ("TODO: Wired in: " ++ show w)

prettyList :: [[Char]] -> [Char]
prettyList xs = "[" ++ intercalate "\n," (map indent1 xs) ++ "]"

prettyDataConId :: ReverseIndex -> DataConId -> String
prettyDataConId index dataConId =
  (case M.lookup dataConId (reverseIndexDataCons index) of
     Nothing -> error ("Couldn't find name! BUG!" ++ show dataConId)
     Just name -> show (displayName name))

prettyArg :: ReverseIndex -> Arg -> [Char]
prettyArg index =
  \case
    VarArg someVarId -> node "VarArg" [prettySomeVarId index someVarId]
    LitArg lit -> node "LitArg" [show lit]

node :: [Char] -> [String] -> [Char]
node a rest = "(" ++ intercalate "\n" (a : map indent rest) ++ ")"

indent :: String -> [Char]
indent = intercalate "\n" . map ("  " ++) . lines

indent1 :: String -> [Char]
indent1 = intercalate "\n" . map (" " ++) . lines
