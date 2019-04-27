{-# LANGUAGE BangPatterns #-}

module FibIterative (it) where

it :: Int
it = fib 50

fib :: Int -> Int
fib n = go 0 1 0
  where
    go !acc0 !acc1 !i
      | i == n = acc0
      | otherwise = go acc1 (acc0 + acc1) (i + 1)
