


f :: Num a => [a] -> [a]
f list = list


help :: Num a => Bool -> [a] -> [a]
help _ [] = []
help True (x:xs) = 2 * x : help False xs
help False (x:xs) = x : help True xs


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = help (length xs `mod` 2 == 0) xs
{-doubleEveryOther (x:xs) = help (length (x:xs) `mod` 2 == 0) (x:xs)-}

quer :: Integer -> Integer
quer 0 = 0
quer x = (x `mod` 10) + quer (x `div` 10) 

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum(map quer xs)

helpToDigits :: Integer -> [Integer]
helpToDigits x
    | x <= 0 = []
    | otherwise      = (x `mod` 10) : helpToDigits (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (helpToDigits x)

validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0

