{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char

parse :: String -> [LogMessage]
parse xs = map parseMessage (lines xs)

parseMessage :: String -> LogMessage
parseMessage xs =
    let parseResult = do
        (messageType, xs1) <- parseMessageType xs
        (number, xs2) <- parseNumber xs1
        Just (LogMessage messageType number (trim xs2))
    in case parseResult of
        Just logMessage -> logMessage
        Nothing -> Unknown xs

trim :: String -> String
trim [] = []
trim (x:xs)
    | x == ' ' = trim xs
    | otherwise = x:xs

parseMessageType :: String -> Maybe (MessageType, String)
parseMessageType ys =
    let ys' = trim ys
    in parseMessageTypeTrimmed ys'

parseMessageTypeTrimmed :: String -> Maybe (MessageType, String)
parseMessageTypeTrimmed ('E':xs) = do 
    (number, trailing) <- parseNumber xs
    Just(Error number, trailing)
parseMessageTypeTrimmed ('I':xs) = Just (Info, xs)
parseMessageTypeTrimmed ('W':xs) = Just (Warning, xs)
parseMessageTypeTrimmed _ = Nothing

parseNumber :: String -> Maybe (Int, String)
parseNumber xs = 
    let trimmed = trim xs
        intChars = takeWhile isDigit trimmed
        trailing = dropWhile isDigit trimmed
    in if (length intChars > 0) then Just (read intChars, trailing) else Nothing

getTime :: LogMessage -> TimeStamp
getTime (LogMessage _ t _) = t 
getTime _ = 0

getText :: LogMessage -> String
getText (LogMessage _ _ t) = t
getText (Unknown t) = t

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left nodeMsg right) =
    let laterThan = (getTime msg) > (getTime nodeMsg)
    in if (laterThan)
        then Node (insert msg left) nodeMsg right 
        else Node left nodeMsg (insert msg right)

build ::  [LogMessage] -> MessageTree
build xs = foldl (flip insert) Leaf xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left nodeMsg right) = inOrder(right) ++ [nodeMsg] ++ inOrder(left)

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages xs = inOrder (build xs)

isError50 :: LogMessage -> Bool
isError50 (LogMessage (Error n) _ _) = if (n >= 50) then True else False
isError50 _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map getText (sortMessages (filter isError50 xs))
