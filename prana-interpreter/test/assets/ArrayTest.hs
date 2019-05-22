{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- | Test some of the ops for Array# and MutableArray#.

module ArrayTest where

import GHC.Exts
import GHC.Types (IO(..))

it :: IO (Char, Int, Char)
it =
  IO
    (\s ->
       case newArray# 22# 'a' s of
         (# s', array #) ->
           case readArray# array 0# s' of
             (# s'', ch #) ->
               case unsafeFreezeArray# array s'' of
                 (# s''', arr #) ->
                   (# s'''
                    , ( ch
                      , I# (sizeofMutableArray# array)
                      , case indexArray# arr 1# of
                          (# x #) -> x)#))
