

module Golf where

import Data.Map (Map)
import qualified Data.Map as Map

{-skips-}
{-Create a list `nats` of natural from 1 to the length of the list (we'll use it twice).-}
{-Every number represents one of the step sizes we use to jump over the list.-}
{-If we now fix a step size x, we observe that we exactly step over the xth, 2*xth, 3*xth, ... elements in the original list.-}
{-We get all multiples of x in nats by filtering for `mod` n == 0.-}
{-Because indexing starts at 0 we subtract 1 from the filtered list and then we use the elements as indexes on the original list.-}
{-We now generalize from the fixed x and take the helper function and map it over all x (=nats) to obtain the desired result.-}


skips :: [a] -> [[a]]
skips [] = []
skips xs = 
    let nats = [1..(length xs)]
        f = \x -> (map ((xs!!) . \z -> z - 1) (filter (\y -> y `mod` x == 0) nats))
    in map f nats


{-localMaxima-}
localMaxima :: [Integer] -> [Integer]
localMaxima xs = helper xs False []


helper :: [Integer] -> Bool -> [Integer] -> [Integer]
helper [] _ results = results
helper (x:[]) _ results = results
helper (x:y:xs) wasBigger results =
    let isMaxima = wasBigger && x > y
        newResults = if isMaxima then results ++ [x] else results
    in helper (y:xs) (y > x) newResults


{-histogram-}
histogram :: [Integer] -> String
histogram xs =
    let countMap = Map.fromList (map (\x -> (toInteger x, toInteger (length (filter (==x) xs)))) [0..9])
        maxCount = maximum (map (\x -> Map.findWithDefault 0 x countMap) [0..9])
        rows = map (\rowNr -> rowString rowNr countMap) (reverse [1..maxCount])
    in (unlines rows) ++ "==========\n0123456789\n"

rowString :: Integer -> (Map Integer Integer) -> String
rowString rowNr countMap = map (\x -> if (Map.findWithDefault 0 x countMap >= rowNr) then '*' else ' ') [0..9]

