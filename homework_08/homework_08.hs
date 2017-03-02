{-# OPTIONS_GHC -fno-warn-orphans #-}
module Homework_08 where

import Employee
import Data.Monoid
import Data.Tree


{-Exercise 1 -}
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) ((empFun e) + fun)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es fun1) (GL fs fun2) = GL (es ++ fs) (fun1 + fun2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL es fun1) (GL fs fun2) = if (fun1 > fun2) then GL es fun1 else GL fs fun2


{-Exercise 2 -}
treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e _ (Node a []) = e
treeFold e f (Node a fs) = f a (map (treeFold e f) fs)

{-Exercise 3 -}
combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs e gs = moreFun (foldr mappend mempty gs) (foldr mappend (GL [e] (empFun e)) gs)

{-Exercise 4 -}
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e glt =
    let withBoss = foldr mappend (GL [e] (empFun e)) (map snd glt)
        withoutBoss = foldr mappend mempty (map fst glt)
    in (withBoss, withoutBoss)

{-Exercise 5 -}
maxFun :: Tree Employee -> GuestList
maxFun tr =
    let pair = treeFold (mempty, mempty) nextLevel tr
    in moreFun (fst pair) (snd pair)

main :: IO()
main = fmap show (fmap maxFun (fmap read (readFile "company.txt")))
{-there is still some work to do ...-}
