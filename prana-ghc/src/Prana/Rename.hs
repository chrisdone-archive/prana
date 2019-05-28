{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Rename the GHC AST to have globally unique names.

module Prana.Rename
  ( renameTopBinding
  , renameId
  , renameName
  , renameDataType
  , displayName
  , stripVersionOut
  , Name(..)
  , Unique(..)
  , RenameFailure(..)
  ) where

import           Control.Exception
import qualified CoreSyn
import           Data.Bifunctor.TH
import           Data.Bitraversable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Typeable
import           Data.Validation
import qualified DataCon
import qualified FastString
import qualified Id
import qualified Module
import qualified Name
import qualified Outputable
import           Prana.Types
import qualified StgSyn
import qualified TyCon
import qualified Unique
import qualified Var

-- | Some failure in the rename process.
data RenameFailure =
  UnexpectedInternalName !Name.Name
  deriving (Eq, Typeable)

instance Exception RenameFailure where
  displayException =
    \case
      UnexpectedInternalName n ->
        "Unexpected internal name: " ++
        (Outputable.showSDocUnsafe (Outputable.ppr n))

instance Show RenameFailure where
  show (UnexpectedInternalName _) = "UnexpectedInternalName"

-- | Rename a data type and its constructors.
renameDataType ::
     Traversable t
  => Module.Module
  -> (Name.Name, t Var.Id)
  -> Validation (NonEmpty RenameFailure) (Name, t Name)
renameDataType m (typeName, typeConstructors) =
  (,) <$> validationNel (renameName m typeName) <*>
  traverse (validationNel . renameId m) typeConstructors

-- | Rename the STG AST to have globally unique names.
renameTopBinding ::
     Module.Module
  -> StgSyn.GenStgTopBinding Var.Id Var.Id
  -> Validation (NonEmpty RenameFailure) (StgSyn.GenStgTopBinding Name Name)
renameTopBinding m =
  bitraverse (validationNel . renameId m) (validationNel . renameId m)

-- | Rename the id to be a globally unique name.
--
-- For some reason, newtype constructors appear in the AST even at
-- this stage. So we replace them with id. I asked ghc-devs, but again
-- got no answer:
-- <https://mail.haskell.org/pipermail/ghc-devs/2019-March/017384.html>
-- So hopefully this just works.
renameId :: Module.Module -> Var.Id -> Either RenameFailure Name
renameId m thing =
  if Name.isInternalName name
    then Left (UnexpectedInternalName name)
    else if isNewtypeConstructor thing
           then Right
                  (Name
                     { namePackage = "base"
                     , nameModule = "GHC.Base"
                     , nameName = "id"
                     , nameUnique = Exported
                     })
           else Right
                  (Name
                     { namePackage = package
                     , nameModule = module'
                     , nameName = name'
                     , nameUnique =
                         if Var.isExportedId thing
                           then Exported
                           else Unexported
                                  (fromIntegral
                                     (Unique.getKey (Unique.getUnique name)))
                     })
  where
    package =
      stripVersionOut
        (FastString.fs_bs
           (Module.unitIdFS (Module.moduleUnitId (Name.nameModule name))))
    module' =
      FastString.fs_bs
        (Module.moduleNameFS (Module.moduleName (Name.nameModule name)))
    name' = FastString.fs_bs (Name.getOccFS name)
    name =
      case Name.nameModule_maybe n of
        Nothing -> qualifyName n
        Just {} -> n
      where
        n = Name.getName thing
    qualifyName :: Name.Name -> Name.Name
    qualifyName n =
      Name.mkExternalName
        (Unique.getUnique n)
        m
        (Name.nameOccName n)
        (Name.nameSrcSpan n)

-- | Rename a general name.
renameName :: Module.Module -> Name.Name -> Either RenameFailure Name
renameName m thing =
  if Name.isInternalName name
    then Left (UnexpectedInternalName name)
    else Right
           (Name
              { namePackage = package
              , nameModule = module'
              , nameName = name'
              , nameUnique = Exported
              })
  where
    package =
      stripVersionOut
        (FastString.fs_bs
           (Module.unitIdFS (Module.moduleUnitId (Name.nameModule name))))
    module' =
      FastString.fs_bs
        (Module.moduleNameFS (Module.moduleName (Name.nameModule name)))
    name' = FastString.fs_bs (Name.getOccFS name)
    name =
      case Name.nameModule_maybe n of
        Nothing -> qualifyName n
        Just {} -> n
      where
        n = Name.getName thing
    qualifyName :: Name.Name -> Name.Name
    qualifyName n =
      Name.mkExternalName
        (Unique.getUnique n)
        m
        (Name.nameOccName n)
        (Name.nameSrcSpan n)

stripVersionOut :: ByteString -> ByteString
stripVersionOut =
  mconcat . intersperse "-" . reverse . stripOut . reverse . S8.split '-'
  where
    stripOut (hash:ver:rest)
      | S8.all (\c -> isDigit c || c == '.') ver
      , S8.all isAlphaNum hash = rest
    stripOut (ver:rest)
      | S8.all (\c -> isDigit c || c == '.') ver = rest
    stripOut xs = xs

--------------------------------------------------------------------------------
-- Predicates

-- | Is this a newtype constructor?
isNewtypeConstructor :: Id.Id -> Bool
isNewtypeConstructor i =
  case Id.isDataConId_maybe i of
    Nothing -> False
    Just dc -> TyCon.isNewTyCon (DataCon.dataConTyCon dc)

--------------------------------------------------------------------------------
-- Orphans: Easily removable if these instances are ever provided
-- upstream.

deriveBifunctor ''StgSyn.GenStgTopBinding
deriveBifunctor ''StgSyn.GenStgBinding
deriveBifunctor ''StgSyn.GenStgRhs
deriveBifunctor ''StgSyn.GenStgExpr
deriving instance Functor StgSyn.GenStgArg
deriving instance Functor CoreSyn.Tickish

deriveBitraversable ''StgSyn.GenStgTopBinding
deriveBitraversable ''StgSyn.GenStgBinding
deriveBitraversable ''StgSyn.GenStgRhs
deriveBitraversable ''StgSyn.GenStgExpr
deriving instance Traversable StgSyn.GenStgArg
deriving instance Traversable CoreSyn.Tickish

deriveBifoldable ''StgSyn.GenStgTopBinding
deriveBifoldable ''StgSyn.GenStgBinding
deriveBifoldable ''StgSyn.GenStgRhs
deriveBifoldable ''StgSyn.GenStgExpr
deriving instance Foldable StgSyn.GenStgArg
deriving instance Foldable CoreSyn.Tickish
