{-# LANGUAGE BangPatterns #-}

module FibCodata (it) where

it :: Int
it = fibs FibCodata.!! 49


fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- | We implement our own Integer-based !! to force use of
-- MultiValAlts -- using unboxed tuples for a loop. The regular
-- Prelude.!! works fine, but tests less of the interpreter than this
-- implementation.
(!!) :: [p] -> Integer -> p
xs !! n
  | otherwise =
    foldr
      (\x r k ->
         case k of
           0 -> x
           _ -> r (k - 1))
      (error "too large")
      xs
      n
