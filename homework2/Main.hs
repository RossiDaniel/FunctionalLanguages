module Main where
import System.IO
import WhatWentWrong
import Parse

main :: IO()
main = do
        content <- getContents
        printLoop (parse content)
        let x = (whatWentWrong. inOrder . build . parse) content
        printLoop x

printLoop :: Show a => [a] -> IO()
printLoop [] = putChar "\n"
printLoop (x:xs) = do 
                   print x
                   printLoop xs