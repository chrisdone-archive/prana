{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE EmptyDataDeriving #-}

-- | FFI-related work.

module Prana.FFI where

import           Data.Bifunctor
import           Data.Function
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Typeable
import           Data.Validation
import qualified Module
import qualified Name
import qualified Outputable
import           Prana.Rename
import           Prana.Types
import qualified Type

data FFIError
  = UnsupportedFFIType !Type.Type
  | ExpectedFunType !Type.Type
  | ExpectedConType !Type.Type
  | NameResolveIssue !RenameFailure
  | ExpectedUnboxedTuple !Type.Type !Name
  | ExpectedUnboxedTupleType
  | ExpectedStateRealWorld !Type.Type
  | UnknownPrimFFIType !WiredInType
  | InvalidFFIType !Name
  deriving (Typeable)

instance Eq FFIError where (==) = on (==) show

instance Show FFIError where
  show ExpectedUnboxedTupleType = "ExpectedUnboxedTupleType"
  show (UnsupportedFFIType ty) = Outputable.showSDocUnsafe (Outputable.ppr ty)
  show (ExpectedFunType ty) =
    "ExpectedFunType: " ++ Outputable.showSDocUnsafe (Outputable.ppr ty)
  show (ExpectedConType ty) =
    "ExpectedConType: " ++ Outputable.showSDocUnsafe (Outputable.ppr ty)
  show (NameResolveIssue ty) = "NameResolveIssue: " ++ show ty
  show (ExpectedUnboxedTuple typ ty) =
    "ExpectedUnboxedTuple: " ++
    Outputable.showSDocUnsafe (Outputable.ppr typ) ++ " " ++ show ty
  show (ExpectedStateRealWorld ty) =
    "ExpectedStateRealWorld: " ++ Outputable.showSDocUnsafe (Outputable.ppr ty)
  show (UnknownPrimFFIType x) = "UnknownPrimFFIType: " ++ show x
  show (InvalidFFIType x) = "InvalidFFIType: " ++ show x

parseAcceptableFFIReturnType ::
     Module.Module
  -> Map Name WiredInType
  -> Type.Type
  -> Validation (NonEmpty FFIError) FFIReturnType
parseAcceptableFFIReturnType theModule wiredInTypes typ =
  case Type.splitTyConApp_maybe typ of
    Just (tyCon, Type.dropRuntimeRepArgs -> (stateRealWorld:args)) -> do
      bindValidation
        (validationNel
           (first NameResolveIssue (renameName theModule (Name.getName tyCon))))
        (\name ->
           case M.lookup name wiredInTypes of
             Just (WiredIn_UnboxedTuple i)
               | i >= 1 ->
                 if Outputable.showSDocUnsafe (Outputable.ppr stateRealWorld) ==
                    "State# RealWorld"
                   then fmap
                          FFIUnboxedTupleOfStateRealWorldAnd
                          (traverse (parseFFIType theModule wiredInTypes) args)
                   else Failure (pure (ExpectedStateRealWorld typ))
             _ -> Failure (pure (ExpectedUnboxedTuple typ name)))
    _ -> Failure (pure ExpectedUnboxedTupleType)

parseFFIType ::
     Module.Module
  -> Map Name WiredInType
  -> Type.Type
  -> Validation (NonEmpty FFIError) FFIType
parseFFIType theModule wiredInTypes typ =
  case Type.splitTyConApp_maybe typ of
    Just (tyCon, Type.dropRuntimeRepArgs -> []) -> do
      bindValidation
        (validationNel
           (first NameResolveIssue (renameName theModule (Name.getName tyCon))))
        (\name ->
           case M.lookup name wiredInTypes of
             Just WiredIn_CharPrimTyConName -> pure FFI_Char
             Just WiredIn_IntPrimTyConName -> pure FFI_Int
             Just WiredIn_Int32PrimTyConName -> pure FFI_Int32
             Just WiredIn_Int64PrimTyConName -> pure FFI_Int64
             Just WiredIn_WordPrimTyConName -> pure FFI_Word
             Just WiredIn_Word32PrimTyConName -> pure FFI_Word32
             Just WiredIn_Word64PrimTyConName -> pure FFI_Word64
             Just WiredIn_AddrPrimTyConName -> pure FFI_Addr
             Just WiredIn_FloatPrimTyConName -> pure FFI_Float
             Just WiredIn_DoublePrimTyConName -> pure FFI_Double
             Just WiredIn_StablePtrPrimTyConName -> pure FFI_StablePtr
             Just ty -> Failure (pure (UnknownPrimFFIType ty))
             Nothing -> Failure (pure (InvalidFFIType name)))
    Just (_, args) ->
      error
        ("Didn't expect args... " ++
         unlines (map (\x -> Outputable.showSDocUnsafe (Outputable.ppr x)) args))
    _ -> Failure (pure ExpectedUnboxedTupleType)
