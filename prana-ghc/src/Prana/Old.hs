-- newtype Convert a =
--   Convert
--     { runConvert :: Validation [ConvertError] a
--     }
--   deriving (Functor, Applicative)

-- failure :: ConvertError -> Convert a
-- failure e = Convert (ReaderT (const (Failure [e])))

-- --------------------------------------------------------------------------------
-- -- Compile given module graph

-- compileModuleGraph :: GHC.ModuleGraph -> GHC.Ghc todo
-- compileModuleGraph = undefined

-- --------------------------------------------------------------------------------
-- -- Update names database

-- withIdDatabase :: (Db -> GHC.Ghc (a, Db)) -> GHC.Ghc a
-- withIdDatabase _cont = undefined

-- --------------------------------------------------------------------------------
-- -- Conversion functions

-- fromGenStgTopBinding :: StgSyn.GenStgTopBinding Var.Id Var.Id -> Convert GlobalBinding
-- fromGenStgTopBinding =
--   \case
--     StgSyn.StgTopLifted genStdBinding ->
--       case genStdBinding of
--         StgSyn.StgNonRec bindr rhs ->
--           GlobalNonRec <$> lookupGlobalVarId bindr <*> fromGenStgRhs rhs
--         StgSyn.StgRec pairs ->
--           GlobalRec <$>
--           traverse
--             (\(bindr, rhs) -> (,) <$> lookupGlobalVarId bindr <*> fromGenStgRhs rhs)
--             pairs
--     StgSyn.StgTopStringLit bindr byteString ->
--       GlobalStringLit <$> lookupGlobalVarId bindr <*> pure byteString

-- fromGenStgBinding :: StgSyn.GenStgBinding Var.Id Var.Id -> Convert LocalBinding
-- fromGenStgBinding =
--   \case
--     StgSyn.StgNonRec bindr rhs ->
--       LocalNonRec <$> lookupLocalVarId bindr <*> fromGenStgRhs rhs
--     StgSyn.StgRec pairs ->
--       LocalRec <$>
--       traverse
--         (\(bindr, rhs) -> (,) <$> lookupLocalVarId bindr <*> fromGenStgRhs rhs)
--         pairs

-- fromGenStgRhs :: StgSyn.GenStgRhs Var.Id Var.Id -> Convert Rhs
-- fromGenStgRhs =
--   \case
--     StgSyn.StgRhsClosure _costCentreStack _binderInfo freeVariables updateFlag parameters expr ->
--       RhsClosure <$> traverse lookupLocalVarId freeVariables <*>
--       pure
--         (case updateFlag of
--            StgSyn.ReEntrant -> ReEntrant
--            StgSyn.Updatable -> Updatable
--            StgSyn.SingleEntry -> SingleEntry) <*>
--       traverse lookupLocalVarId parameters <*>
--       fromStgGenExpr expr
--     StgSyn.StgRhsCon _costCentreStack dataCon arguments ->
--       RhsCon <$> lookupDataConId dataCon <*> traverse fromStgGenArg arguments

-- fromStgGenArg :: StgSyn.GenStgArg Var.Id -> Convert Arg
-- fromStgGenArg =
--   \case
--     StgSyn.StgVarArg occ -> VarArg <$> lookupSomeVarId occ
--     StgSyn.StgLitArg _literal -> pure (LitArg Lit)

-- fromStgGenExpr :: StgSyn.GenStgExpr Var.Id Var.Id -> Convert Expr
-- fromStgGenExpr =
--   \case
--     StgSyn.StgApp occ arguments ->
--       AppExpr <$> lookupSomeVarId occ <*> traverse fromStgGenArg arguments
--     StgSyn.StgLit literal -> LitExpr <$> pure (const Lit literal)
--     StgSyn.StgConApp dataCon arguments types ->
--       ConAppExpr <$> lookupDataConId dataCon <*> traverse fromStgGenArg arguments <*>
--       pure (map (const Type) types)
--     StgSyn.StgOpApp stgOp arguments typ ->
--       OpAppExpr <$> pure (const Op stgOp) <*> traverse fromStgGenArg arguments <*>
--       pure (const Type typ)
--     StgSyn.StgCase expr bndr altType alts ->
--       CaseExpr <$> fromStgGenExpr expr <*> lookupLocalVarId bndr <*>
--       case altType of
--         StgSyn.PolyAlt
--           | [(CoreSyn.DEFAULT, [], rhs)] <- alts ->
--             PolymorphicAlt <$> fromStgGenExpr rhs
--           | otherwise -> failure UnexpectedPolymorphicCaseAlts
--         StgSyn.MultiValAlt count ->
--           (\(mdef, dataAlts) -> MultiValAlts count dataAlts mdef) <$>
--           fromAltTriples alts
--         StgSyn.AlgAlt tyCon -> do
--           (\(mdef, dataAlts) -> DataAlts (const TyCon tyCon) dataAlts mdef) <$>
--             fromAltTriples alts
--         StgSyn.PrimAlt primRep -> do
--           (\(mdef, primAlts) -> PrimAlts (const PrimRep primRep) primAlts mdef) <$>
--             fromPrimAltTriples alts
--     StgSyn.StgLet binding expr ->
--       LetExpr <$> fromGenStgBinding binding <*> fromStgGenExpr expr
--     StgSyn.StgLetNoEscape binding expr ->
--       LetExpr <$> fromGenStgBinding binding <*> fromStgGenExpr expr
--     StgSyn.StgTick _tickish expr -> fromStgGenExpr expr
--     StgSyn.StgLam {} -> failure UnexpectedLambda

-- fromAltTriples :: [StgSyn.GenStgAlt Var.Id Var.Id] -> Convert (Maybe Expr, [DataAlt])
-- fromAltTriples alts = do
--   let mdef =
--         listToMaybe
--           (mapMaybe
--              (\case
--                 (CoreSyn.DEFAULT, [], e) -> Just e
--                 _ -> Nothing)
--              alts)
--       adtAlts =
--         mapMaybe
--           (\case
--              (CoreSyn.DataAlt dc, bs, e) -> pure (dc, bs, e)
--              _ -> Nothing)
--           alts
--   (,) <$> maybe (pure Nothing) (fmap Just . fromStgGenExpr) mdef <*>
--     traverse
--       (\(dc, bs, e) ->
--          DataAlt <$> lookupDataConId dc <*> traverse lookupLocalVarId bs <*>
--          fromStgGenExpr e)
--       adtAlts

-- fromPrimAltTriples :: [StgSyn.GenStgAlt Var.Id Var.Id] -> Convert (Maybe Expr, [LitAlt])
-- fromPrimAltTriples alts = do
--   let mdef =
--         listToMaybe
--           (mapMaybe
--              (\case
--                 (CoreSyn.DEFAULT, [], e) -> Just e
--                 _ -> Nothing)
--              alts)
--       adtAlts =
--         mapMaybe
--           (\case
--              (CoreSyn.LitAlt dc, bs, e) -> pure (dc, bs, e)
--              _ -> Nothing)
--           alts
--   (,) <$> maybe (pure Nothing) (fmap Just . fromStgGenExpr) mdef <*>
--     traverse
--       (\(dc, bs, e) ->
--          LitAlt <$> pure (const Lit dc) <*> traverse lookupLocalVarId bs <*>
--          fromStgGenExpr e)
--       adtAlts

-- --------------------------------------------------------------------------------
-- -- Bindings and names

-- lookupDataConId :: DataCon.DataCon -> Convert DataConId
-- lookupDataConId = error "lookupDataConId"

-- lookupSomeVarId :: Var.Id -> Convert SomeVarId
-- lookupSomeVarId varId =
--   if Var.isExportedId varId
--     then fmap SomeExportedVarId (lookupExportedVarId varId)
--     else fmap SomeLocalVarId (lookupLocalVarId varId)

-- -- TODO: use toExportedNormalized
-- lookupExportedVarId :: Var.Id -> Convert ExportedVarId
-- lookupExportedVarId = error "lookupExportedVarId"

-- -- TODO: use toLocalNormalized
-- lookupLocalVarId :: Var.Id -> Convert LocalVarId
-- lookupLocalVarId = error "lookupLocalVarId"
