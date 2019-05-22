{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- |

module ArrayTest where

import GHC.Exts
import GHC.Types (IO(..))

it :: IO Char
it = IO (\s -> case newArray# 1# 'a' s of
                 (# s', array #) -> readArray# array 0# s')
