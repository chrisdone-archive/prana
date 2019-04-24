{-# LANGUAGE BangPatterns #-}

module Fib (it) where

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

(!!) :: [a] -> Int -> a
xs !! n =
  foldr
    (\x r k ->
       case k of
         0 -> x
         _ -> r (k - 1))
    (error "too large")
    xs
    n

it :: Int
it = fibs Fib.!! 49
