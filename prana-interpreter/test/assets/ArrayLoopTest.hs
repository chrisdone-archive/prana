{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- | Tests that boxed things written to the array don't loop.

module ArrayLoopTest where

import GHC.Exts
import GHC.Types (IO(..))

it :: IO (Char, Int, Char)
it =
  IO
    (\s ->
       case newArray# 22# (let x = x
                           in x :: Char) s of
         (# s', array #) ->
           case readArray# array 0# s' of
             (# s'', ch #) ->
               case unsafeFreezeArray# array s'' of
                 (# s''', arr #) ->
                   (# s'''
                    , ( 'a' -- Replacing this with 'ch' causes a loop, which is proper.
                      , I# (sizeofMutableArray# array)
                      , case indexArray# arr 1# of
                          (# x #) -> 'a')#))
