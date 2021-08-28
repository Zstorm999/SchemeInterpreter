module Evaluator (
    eval
) where


import Environment
import Errors
import Values

import Evaluator.Primitives

import Control.Monad

eval :: Env -> LispVal -> IOThrowsError LispVal
--primitive types evaluation 
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Character _) = return val
eval env val@(Bool _) = return val

-- quotations
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "quasiquote", val]) = evalQuasiQuote val
    where
        evalQuasiQuote (List [Atom "unquote", val]) = eval env val
        evalQuasiQuote (List elements) = List <$> mapM evalQuasiQuote elements
        evalQuasiQuote (DottedList elements last) = do
                                                        head <- mapM evalQuasiQuote elements
                                                        tail <- evalQuasiQuote last 
                                                        return $ DottedList head tail
        evalQuasiQuote any = return any

-- control flow
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        _  -> eval env conseq

-- variables 
eval env (Atom id) = getVar env id

eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var 

-- functions
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
    where apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                            ($ args)
                            (lookup func primitives)


-- bad form
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

