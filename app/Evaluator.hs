module Evaluator (
    eval
) where

import Errors
import Values

import Evaluator.Primitives

import Control.Monad

eval :: LispVal -> ThrowsError LispVal
--primitive types evaluation 
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Character _) = return val
eval val@(Bool _) = return val

-- quotations
eval (List [Atom "quote", val]) = return val
eval (List [Atom "quasiquote", val]) = evalQuasiQuote val
    where
        evalQuasiQuote :: LispVal -> ThrowsError LispVal
        evalQuasiQuote (List [Atom "unquote", val]) = eval val
        evalQuasiQuote (List elements) = List <$> mapM evalQuasiQuote elements
        evalQuasiQuote (DottedList elements last) = do
                                                        head <- mapM evalQuasiQuote elements
                                                        tail <- evalQuasiQuote last 
                                                        return $ DottedList head tail
        evalQuasiQuote any = return any

-- control flow
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        _  -> eval conseq

-- functions
eval (List (Atom func : args)) = mapM eval args >>= apply func
    where apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                            ($ args)
                            (lookup func primitives)


-- bad form
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

