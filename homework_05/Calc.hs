module Calc where


import ExprT
import Parser

{-Exercise 1-}
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add exp1 exp2) = eval(exp1) + eval(exp2)
eval (Mul exp1 exp2) = eval(exp1) * eval(exp2)


{-Exercise 2-}
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul


{-Exercise 3-}
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id


{-Exercise 4-}
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit n 
        | n <= 0 = False
        | otherwise = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Ord, Show)
instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ( ((+) x y) `mod` 7 )
    mul (Mod7 x) (Mod7 y) = Mod7 ( ((*) x y) `mod` 7 )

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7
