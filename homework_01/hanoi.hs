

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n <= 0 = []
    | n == 1 = [(a, b)]
    | n == 2 = [(a, c), (a, b), (c, b)]
    | otherwise = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)

hanoi3Length :: Integer -> Int
hanoi3Length n = length (hanoi n "a" "b" "c")

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
    | n <= 0 = []
    | n == 1 = [(a, b)]
    | n == 2 = [(a, c), (a, b), (c, b)]
    | n == 3 = [(a, c), (a, d), (a, b), (d, b), (c, b)]
    | n == 4 = [(a, d), (a, b), (a, c), (b, c), (a, b), (c, a), (c, b), (a, b), (d, b)]
    | n == 5 = (hanoi4 3 a d c b) ++ (hanoi 2 a b c) ++ (hanoi4 3 d b a c)
    | n == 6 = (hanoi4 3 a d c b) ++ (hanoi 3 a b c) ++ (hanoi4 3 d b a c)
    | n == 7 = (hanoi4 4 a d c b) ++ (hanoi 3 a b c) ++ (hanoi4 4 d b a c)
    | n == 8 = (hanoi4 5 a d c b) ++ (hanoi 3 a b c) ++ (hanoi4 5 d b a c)
    | n == 9 = (hanoi4 6 a d c b) ++ (hanoi 3 a b c) ++ (hanoi4 6 d b a c)
    | n == 10 = (hanoi4 6 a d c b) ++ (hanoi 4 a b c) ++ (hanoi4 6 d b a c)
    | n == 11 = (hanoi4 7 a d c b) ++ (hanoi 4 a b c) ++ (hanoi4 7 d b a c)
    | n == 12 = (hanoi4 8 a d c b) ++ (hanoi 4 a b c) ++ (hanoi4 8 d b a c)
    | n == 13 = (hanoi4 9 a d c b) ++ (hanoi 4 a b c) ++ (hanoi4 9 d b a c)
    | otherwise = (hanoi4 (n - 5) a d c b) ++ (hanoi 5 a b c) ++ (hanoi4 (n - 5) d b a c)

hanoi4Length :: Integer -> Int
hanoi4Length n = length (hanoi4 n "a" "b" "c" "d")
