module Evaluator (
    eval
) where


import Environment
import Values

import Evaluator.Primitives

import Control.Monad.Except
import Data.Maybe (isNothing)

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
eval env (List (Atom "define" : List (Atom var : params) : body )) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs env [] body 
eval env (List (function : args)) = do
    func <- eval env function 
    argValues <- mapM (eval env) args 
    apply func argValues

-- bad form
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && isNothing varargs
        then throwError $ NumArgs (num params) args
        else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody

    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = last <$> mapM (eval env) body
          bindVarArgs arg env = case arg of
                                    Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                                    Nothing -> return env

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal 
makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing 
makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs = makeFunc . Just . show


