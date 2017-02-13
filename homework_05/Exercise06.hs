{-# LANGUAGE FlexibleInstances #-}

module Exercise06 where


import qualified Data.Map as M


class HasVars a where
    var :: String -> a

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a


data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var _ = Lit 5


instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = \m -> Just n
    add x y = \m -> (+) <$> (x m) <*> (y m)
    mul x y = \m -> (*) <$> (x m) <*> (y m)

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
