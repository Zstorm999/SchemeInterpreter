module Evaluator.Primitives.Number (
    primitives
) where

import Errors
import Values

import Evaluator.Primitives.Operations

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("number?", unOp isNumber),
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("%", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=))]


-- numbers
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op parameters = mapM unpackNum parameters >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

numBoolBinop = boolBinop unpackNum

isNumber :: LispVal -> ThrowsError LispVal
isNumber (Number _) = return $ Bool True
isNumber _ = return $ Bool False
