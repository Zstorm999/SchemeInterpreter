module Evaluator.Primitives.List (
    primitives
) where

import Values

import Evaluator.Primitives.Operations


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("car", unOp car),
    ("cdr", unOp cdr),
    ("cons", cons)]

car :: LispVal -> ThrowsError LispVal
car (List (x:xs)) = return x
car (DottedList (x:xs) _) = return x
car badArgs = throwError $ TypeMismatch "pair" badArgs

cdr :: LispVal -> ThrowsError LispVal
cdr (List [_,x]) = return x
cdr (List (x:xs)) = return $ List xs
cdr (DottedList [xs] x) = return x
cdr (DottedList (_:xs) x) = return $ DottedList xs x
cdr badArgs = throwError $ TypeMismatch "pair" badArgs

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x:xs
cons [x, DottedList xs xLast] = return $ DottedList (x:xs) xLast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList