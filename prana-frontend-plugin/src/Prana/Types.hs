{-# LANGUAGE DeriveDataTypeable #-}
-- |

module Prana.Types where

import Data.ByteString (ByteString)
import Data.Data
import Data.Function
import Data.Set (Set)

-- | Some binding declared top-level in a module.
data Binding = Binding
  { bindingId :: {-# UNPACK #-}!BindingId -- ^ A unique ID for this binding.
  , bindingFlagged :: !(Set ByteString) -- ^ This binding was flagged by a predicate.
  , bindingSrcSpan :: !(Maybe Span) -- ^ Location for the binding.
  , bindingRefs :: ![BindingId] -- ^ Bindings that I reference.
  } deriving (Show, Data, Typeable)

instance Eq Binding where (==) = on (==) bindingId
instance Ord Binding where compare = on compare bindingId

-- | Source span.
data Span = Span
  { spanFile :: {-# UNPACK #-}!ByteString
  , spanStartLine :: !Int
  , spanStartCol :: !Int
  , spanEndLine :: !Int
  , spanEndCol :: !Int
  } deriving (Show, Data, Typeable)

-- | ID for a binding declared in some package, in some module, with
-- some name.
data BindingId = BindingId
  { bindingIdPackage :: !ByteString
  , bindingIdModule :: !ByteString
  , bindingIdName :: !ByteString
  } deriving (Show, Ord, Eq, Data, Typeable)
