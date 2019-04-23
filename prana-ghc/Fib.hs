{-# LANGUAGE BangPatterns #-}

module Fib (it) where

fib :: Int -> Int
fib n = go 0 1 0
  where
    go !acc0 !acc1 !i
      | i == n = acc0
      | otherwise = go acc1 (acc0 + acc1) (i + 1)

-- it :: Int
-- it = fib 50

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

taker :: Int -> [Int] -> [Int]
taker count xs = go 0 xs
  where go !cur  _ | cur == count = []
        go !_    []     = []
        go !cur  (x:xs) = x : go (cur+1) xs

xs !! n
  | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) (error "too large") xs n



it :: [Int]
it = taker 50 fibs
-- it :: Int
-- it = fibs Fib.!! 1


{-

Reproducible test case!

-}

callBinaryFunc :: (t -> t1 -> t2) -> t2
callBinaryFunc f = f undefined undefined

-- it :: ()
-- it = callBinaryFunc (\_ _ -> \_ -> ()) undefined
