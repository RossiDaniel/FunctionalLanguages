module WhatWentWrong where

import Log

example :: [LogMessage]
example = [LogMessage Info 6 "Completed armadillo processing",LogMessage Info 1 "Nothing to report",LogMessage Info 4 "Everything normal",LogMessage Info 11 "Initiating self-destruct sequence",LogMessage (Error 70) 3 "Way too many pickles",LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",LogMessage Warning 5 "Flange is due for a check-up",LogMessage Info 7 "Out for lunch, back in two time steps",LogMessage (Error 20) 2 "Too many pickles",LogMessage Info 9 "Back from lunch",LogMessage (Error 99) 10 "Flange failed!"]

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert (LogMessage ty tmp str) (Leaf) = Node (Leaf) (LogMessage ty tmp str) (Leaf)
insert (LogMessage ty tmp str) (Node l (Unknown _) r) = Node l (LogMessage ty tmp str) r
insert (LogMessage ty tmp str) (Node l (LogMessage nty ntmp nstr) r) | tmp <= ntmp = Node (insert (LogMessage ty tmp str) l) (LogMessage nty ntmp nstr) r
                                                                     | otherwise   = Node l (LogMessage nty ntmp nstr) (insert (LogMessage ty tmp str) r)

build :: [LogMessage] -> MessageTree
build ls = foldl (\x y -> insert y x) (Leaf) ls

inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node l (Unknown _) r) = (inOrder l) ++ (inOrder r)
inOrder (Node l (LogMessage nty ntmp nstr) r) = (inOrder l) ++ [(LogMessage nty ntmp nstr)] ++ (inOrder r)

higherror :: LogMessage -> Bool
higherror (LogMessage (Error n) _ _) | n >= 50   = True
                                     | otherwise = False
higherror _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ls = [nstr | (LogMessage nty ntmp nstr) <- (inOrder (build ls)), higherror (LogMessage nty ntmp nstr)]

