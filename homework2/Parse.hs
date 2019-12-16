{-# OPTIONS_GHC -Wall #-}
module Parse where
import Log
import Data.Char

str2int :: String -> Int
str2int xs = foldl (\x y -> x*10 + (digitToInt y)) 0 xs

logmes :: MessageType -> Int -> String -> LogMessage
logmes tp tmp str = LogMessage (tp) (tmp) (str)

strisint :: String -> Bool
strisint str | (length str) == 0 = False
             | otherwise         = and [isDigit x | x <- str]

validator :: [String] -> LogMessage
validator  istr | length istr < 3 = Unknown (unwords istr)
                | (istr!!0) == "I" && strisint (istr!!1) = logmes (Info) (str2int (istr!!1)) (unwords (drop 2 istr))
                | (istr!!0) == "W" && strisint (istr!!1) = logmes (Warning) (str2int (istr!!1)) (unwords (drop 2 istr))
                | (istr!!0) == "E" && strisint (istr!!1) && strisint (istr!!2) = logmes (Error (str2int (istr!!1))) (str2int (istr!!2)) (unwords (drop 3 istr))
                | otherwise = Unknown (unwords istr)

parse :: String -> [LogMessage]
parse [] = []
parse xs = [ validator r | r <- res]
         where
             res = [words n | n <- (lines xs)]