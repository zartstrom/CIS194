

import Data.List

{-Exercise 1-}

{-no style-}
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

{-wholemeal style-}
fun1' :: [Integer] -> Integer
fun1' = product . map ((-) 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=0) . iterate jump

jump :: Integer -> Integer
jump n = if (even n) then (n `div` 2) else if (n==1) then 0 else (3 * n + 1)


{-Exercise 2-}

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (\a b -> myInsert a b) Leaf

myInsert :: a -> Tree a -> Tree a
myInsert a Leaf = Node 0 Leaf a Leaf
myInsert a (Node n left a' right) =
    let insertLeft = elements left < elements right
        updatedTree = if insertLeft then myInsert a left else myInsert a right
        newHeight = max n (height updatedTree + 1)
    in if insertLeft
        then Node newHeight updatedTree a' right
        else Node newHeight left a' updatedTree

height :: Tree a -> Integer
height Leaf = 0
height (Node n _ _ _) = n

elements :: Tree a -> Integer
elements Leaf = 0
elements (Node n left _ right) = elements left + 1 + elements right


{-Exercise 3-}

xor :: [Bool] -> Bool
xor = foldr (\x y -> if(x) then not y else y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f(a):b) []


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)


{-Exercise 4-}
tupleProduct :: (Integer, Integer) -> Integer
tupleProduct t =  fst t + snd t + 2 * fst t * snd t

tuples :: Integer -> [(Integer, Integer)]
tuples n = [(y, x) | x <- [1..n], y <- [1..x]]

candidates :: Integer -> [Integer]
candidates n =
    let notNeeded = filter (<=n) (map tupleProduct (tuples n))
    in [1..n] \\ notNeeded

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (+1) . map (*2) . candidates
