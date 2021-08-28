module Main where

import Control.Monad.Except 
import Data.IORef
import Data.Functor
import System.Environment (getArgs)



import Errors
import Evaluator ( eval )
import Parser ( parseExpr )
import Values ( LispVal )
import REPL




main :: IO()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> putStrLn "Reading of files is not yet supported"
        _ -> putStrLn "Run with no arguments for interactive scheme, or with the path to one scheme source file"

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool 
isBound envRef var = readIORef envRef <&> (maybe False (const True) . lookup var)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do 
    env <- liftIO $ readIORef envRef 
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do 
    env <- liftIO $ readIORef envRef 
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef value))
          (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal 
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
        then setVar envRef var value >> return value 
        else liftIO $ do 
            valueRef <- newIORef value 
            env <- readIORef envRef 
            writeIORef envRef ((var, valueRef) : env)
            return value 



-- env exception handling

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

