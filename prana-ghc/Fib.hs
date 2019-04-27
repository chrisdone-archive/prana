{-# LANGUAGE BangPatterns #-}

module Fib (it) where

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

it :: Int
it = fibs !! 49
