module Main where

import Parser.Combinator
import Lang
import Syntax
import System.IO

repl :: IO ()
repl = do
        putStr "> " >> hFlush stdout
        line <- getLine
        case line of
            "exit" -> putStrLn "exit"
            _ -> case parse stmt line of
                    Success (a, _) -> do
                        print a
                    f@(Failure _ _) -> print f

main :: IO ()
main = repl
