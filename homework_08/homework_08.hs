{-# OPTIONS_GHC -fno-warn-orphans #-}
module Homework_08 where

import Employee
import Data.Monoid
import Data.Tree


glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) ((empFun e) + fun)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es fun1) (GL fs fun2) = GL (es ++ fs) (fun1 + fun2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL es fun1) (GL fs fun2) = if (fun1 > fun2) then GL es fun1 else GL fs fun2


treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold = go where
    go (Node x ts) = f x (map go ts)
