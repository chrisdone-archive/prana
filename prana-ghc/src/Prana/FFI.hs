{-# LANGUAGE EmptyDataDeriving #-}

-- | FFI-related work.

module Prana.FFI where

import           Control.Applicative
import           Data.Function
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Typeable
import           Data.Validation
import qualified Outputable
import           Prana.Types
import qualified Type

data FFIError
  = UnsupportedFFIType !Type.Type
  | ExpectedFunType !Type.Type
  | ExpectedConType !Type.Type
  deriving (Typeable)

instance Eq FFIError where (==) = on (==) show

instance Show FFIError where
  show (UnsupportedFFIType ty) = Outputable.showSDocUnsafe (Outputable.ppr ty)

parseAcceptableFFIReturnType ::
     Type.Type -> Validation (NonEmpty FFIError) FFIReturnType
parseAcceptableFFIReturnType typ =
  parseConAppType typ <> parseFunType typ <> die
  where
    die = Failure (pure (UnsupportedFFIType typ))

parseConAppType :: Type.Type -> Validation (NonEmpty FFIError) FFIReturnType
parseConAppType typ =
  case Type.splitTyConApp_maybe typ of
    Nothing -> Failure (pure (ExpectedConType typ))
    Just (tycon, args) -> undefined

parseFunType :: Type.Type -> Validation (NonEmpty FFIError) FFIReturnType
parseFunType typ =
  bindValidation (maybe (Failure (pure (ExpectedFunType typ))) pure (Type.splitFunTy_maybe typ))
                 (\(x,y) -> )


-- http://hackage.haskell.org/package/ghc-8.6.5/docs/Type.html

-- tyConAppTyCon_maybe :: Type -> Maybe TyCon
-- splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
