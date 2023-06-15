{-#OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{- including data MessageType, 
    type TimeStamp,
    and data LogMessage  -}

import Data.List.Split()

isNum :: String -> Bool
isNum s = case reads s :: [(Integer, String)] of
    [(_, "")]   -> True
    _           -> False
 

parseMessage:: String -> LogMessage
parseMessage msg = (par . splitAt 2 . words) msg where
    par (["E", n] , xs) | isNum $ head xs = LogMessage (Error (read n::Int)) (read (head xs)::Int) (unwords $ tail xs)
    par (["I", n] , xs) | isNum n = LogMessage Info (read n::Int) (unwords xs)
    par (["W", n] , xs) | isNum n = LogMessage Warning (read n::Int) (unwords xs)
    par _ = Unknown msg
    
    
parse :: String -> [LogMessage]
parse = map parseMessage . lines



newNode:: LogMessage -> MessageTree
newNode a = Node Leaf a Leaf

instance Ord LogMessage where
    (LogMessage _ ts1 _) `compare` (LogMessage _ ts2 _) = ts1 `compare` ts2
    _ `compare` _ = error "Compare error"


-- Exercise 2
insert:: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = newNode message
insert message (Node Leaf node rightChild)
        |  message < node = Node (newNode message) node rightChild;
insert message (Node leftChild node Leaf)
        |  message > node = Node leftChild node (newNode message)
insert message (Node leftChild node rightChild)
        |  message < node = Node (insert message leftChild) node rightChild
        |  otherwise = Node leftChild node (insert message rightChild)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftChild node rightChild) = (inOrder leftChild) ++ [node] ++ (inOrder rightChild)

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map messageOf . filter wrong where
    messageOf:: LogMessage -> String
    messageOf (LogMessage _ _ s) = s
    -- messageOf _ = error "No message, huh?"

    wrong:: LogMessage -> Bool
    wrong (LogMessage (Error s) _ _) = s > 50
    -- wrong (LogMessage _ s _) = s > 50
    wrong _ = False


