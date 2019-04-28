{-# LANGUAGE BangPatterns #-}

module FibCodata (it) where

it :: Int
it = fibs FibCodata.!! 49


fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

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
