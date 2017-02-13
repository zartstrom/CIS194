{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Exercise05 where


import StackVM
import Parser


class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a


{-Exercise 5-}
instance Expr Program where
    lit n = [PushI n]
    add x y = x ++ y ++ [Add]
    mul x y = x ++ y ++ [Mul]

reifyProgram :: Program -> Program
reifyProgram = id

compile :: String -> Maybe Program
compile = fmap reifyProgram . parseExp lit add mul
