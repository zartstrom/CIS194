

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
foldTree = foldr (\a b -> insert a b) Leaf

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node n left a' right) =
    let insertLeft = height left < height right
        insertTree = if insertLeft then left else right
        updatedTree = insert a insertTree
        newHeight = max n (height updatedTree + 1)
    in if insertLeft
        then Node newHeight updatedTree a' right
        else Node newHeight left a' updatedTree

height :: Tree a -> Integer
height Leaf = 0
height (Node n _ _ _) = n
