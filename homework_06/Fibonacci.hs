module Fibonacci where


{-Exercise 1-}
fib :: Integer -> Integer
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = (fib (n - 2)) + (fib (n - 1))


fibs1 :: [Integer]
fibs1 = map fib [0..]


{-Exercise x-}
h :: Integer -> Integer -> [Integer]
h m n = m : h n (m + n)

fibs2 :: [Integer]
fibs2 = h 0 1
