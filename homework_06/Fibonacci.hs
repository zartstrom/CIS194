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
