module Evaluator.Primitives.Char (
    primitives
) where 

import Evaluator.Primitives.Operations 
import Values



primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("char?", unOp isChar),
    ("char=?", charBoolBinop (==)),
    ("char<?", charBoolBinop (<)),
    ("char>?", charBoolBinop (>)),
    ("char>=?", charBoolBinop (>=)),
    ("char<=?", charBoolBinop (<=))]


unpackChar :: LispVal -> ThrowsError Char
unpackChar (Character val) = return val
unpackChar notChar = throwError $ TypeMismatch "character" notChar

charBoolBinop :: (Char -> Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charBoolBinop = boolBinop unpackChar

isChar :: LispVal -> ThrowsError LispVal
isChar (Character _) = return $ Bool True
isChar _ = return $ Bool False