{-# LANGUAGE BangPatterns #-}

module Fib (it) where

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

xs !! n
  | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) (error "too large") xs n


fib :: Int -> Int
fib n = go 0 1 0
  where
    go !acc0 !acc1 !i
      | i == n = acc0
      | otherwise = go acc1 (acc0 + acc1) (i + 1)

it :: Int
it = fibs Fib.!! 50
