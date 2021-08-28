module REPL (
    runRepl
) where 

import System.IO (hFlush, stdout)
import Text.ParserCombinators.Parsec (parse)

import Errors
import Evaluator
import Parser
import Values
 

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
    result <- prompt
    if pred result 
        then return ()
        else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = do
    putStrLn "Welcome to Scheme REPL, written by Zstorm999"
    putStrLn "Type \"quit\" to exit the prompt"
    until_ (== "quit") (readPrompt "Scheme>> ") evalAndPrint