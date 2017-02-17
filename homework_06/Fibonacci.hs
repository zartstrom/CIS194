{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where


{-Exercise 1-}
fib :: Integer -> Integer
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = (fib (n - 2)) + (fib (n - 1))


fibs1 :: [Integer]
fibs1 = map fib [0..]


{-Exercise 2-}
h :: Integer -> Integer -> [Integer]
h m n = m : h n (m + n)

fibs2 :: [Integer]
fibs2 = h 0 1


{-Exercise 3-}
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show s = show (take 20 (streamToList s))


{-Exercise 4-}
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x sx) = Cons (f x) (streamMap f sx)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))


{-Exercise 5-}
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

interleaveRuler :: Integer -> Stream Integer
interleaveRuler n = interleaveStreams (streamRepeat n) (interleaveRuler (n + 1))

ruler :: Stream Integer
ruler = interleaveRuler 0


{-Exercise 6-}
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

tailStream :: Stream Integer -> Stream Integer
tailStream (Cons _ xs) = xs

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate (Cons x xs) = Cons (-1 * x) (negate xs)
    (+) (Cons x xs) (Cons y ys) = Cons (x + y) ((+) xs ys)
    (*) (Cons x xs) (Cons y ys) = Cons (x * y) ((streamMap (*x) ys) + ((*) xs (Cons y ys)))


q :: Stream Integer -> Stream Integer -> Stream Integer
q (Cons x xs) (Cons y ys) = Cons (x `div` y) ( streamMap (`div` y) (xs - (q (Cons x xs) (Cons y ys)) * ys) )

instance Fractional (Stream Integer) where
    (/) xs ys = q xs ys

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)


{-Exercise 7-}
data Matrix a = Matrix a a a a deriving Show

instance Num a => Num (Matrix a) where
    fromInteger n = Matrix (fromInteger n) (fromInteger n) (fromInteger n) (fromInteger n)
    (*) (Matrix a b c d) (Matrix e f g h) = Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

p :: Matrix a -> a
p (Matrix _ _ x _) = x

m :: Matrix Integer
m = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 n
    | n == 0 = 0
    | otherwise = p (m^n)
