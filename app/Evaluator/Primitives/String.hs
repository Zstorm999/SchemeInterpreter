module Evaluator.Primitives.String (
    primitives
) where 

 
import Evaluator.Primitives.Operations 
import Values



primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("string?", unOp isString),
    ("string=?", stringBoolBinop (==)),
    ("string>?", stringBoolBinop (>)),
    ("string<?", stringBoolBinop (<)),
    ("string>=?", stringBoolBinop (>=)),
    ("string<=?", stringBoolBinop (<=))]
    
    
-- string
unpackString :: LispVal -> ThrowsError String
unpackString (String val) = return val
unpackString notString = throwError $ TypeMismatch "string" notString

stringBoolBinop = boolBinop unpackString

isString :: LispVal -> ThrowsError LispVal
isString (String _) = return $ Bool True
isString _ = return $ Bool False
