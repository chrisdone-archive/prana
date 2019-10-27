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
    RhsClosure con -> node "RhsClosure" [prettyClosure index con]

prettyClosure :: ReverseIndex -> Closure -> [Char]
prettyClosure index (Closure vars _flag parms expr) =
  node
    "Closure"
    [ prettyList (map (prettyLocalVar index) vars)
    , "UpdateFlag"
    , prettyList (map (prettyLocalVar index) parms)
    , prettyExpr index expr
    ]

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
    OpAppExpr op args ->
      node
        "OpAppExpr"
        [show op, prettyList (map (prettyArg index) args)]
    CaseExpr expr localVarId alts ->
      node
        "CaseExpr"
        [ prettyExpr index expr
        , prettyLocalVar index localVarId
        , prettyAlts index alts
        ]
    LetExpr binding expr ->
      node "LetExpr" [prettyBinding index binding, prettyExpr index expr]
    LitExpr lit -> show lit

prettyBinding :: ReverseIndex -> LocalBinding -> [Char]
prettyBinding index =
  \case
    LocalNonRec v rhs ->
      node "LocalNonRec" [prettyLocalVar index v, prettyRhs index rhs]
    LocalRec vs ->
      node
        "LocalRec"
        [ prettyList
            (map
               (\(v, rhs) ->
                  prettyList [prettyLocalVar index v, prettyRhs index rhs])
               vs)
        ]

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
    Nothing -> error "Couldn't find local name! BUG!"
    Just name -> show (displayName name)

prettySomeVarId :: ReverseIndex -> SomeVarId -> String
prettySomeVarId index =
  \case
    SomeLocalVarId localVarId -> prettyLocalVar index localVarId
    SomeGlobalVarId globalVarId ->
      (case M.lookup globalVarId (reverseIndexGlobals index) of
         Nothing -> error "Couldn't find global name! BUG!"
         Just name -> show (displayName name))
    w@WiredInVal {} -> show w -- error ("TODO: Wired in: " ++ show w)

prettyList :: [[Char]] -> [Char]
prettyList xs = "[" ++ intercalate "\n," (map indent1 xs) ++ "]"

prettyDataConId :: ReverseIndex -> DataConId -> String
prettyDataConId index dataConId =
  (case M.lookup dataConId (reverseIndexDataCons index) of
     Nothing ->
       case dataConId of
         UnboxedTupleConId n -> "(#" ++ replicate (n - 1) ',' ++ "#)"
         _ -> error ("Couldn't find data con name! BUG!" ++ show dataConId)
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
