module Main where


import System.Environment (getArgs)



import Evaluator ( eval )
import Parser ( parseExpr )
import Values ( LispVal )
import REPL




main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> putStrLn "Reading of files is not yet supported"
        _ -> putStrLn "Run with no arguments for interactive scheme, or with the path to one scheme source file"






