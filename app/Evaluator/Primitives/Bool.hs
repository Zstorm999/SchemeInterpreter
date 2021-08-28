module Evaluator.Primitives.Bool (
    primitives
) where 

import Errors 
import Evaluator.Primitives.Operations 
import Values



primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("boolean?", unOp isBool),
    ("not", unOp invertBool),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||))]

    -- bool 
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool val) = return val
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

boolBoolBinop = boolBinop unpackBool

isBool :: LispVal -> ThrowsError LispVal
isBool (Bool _) = return $ Bool True
isBool _ = return $ Bool False

invertBool :: LispVal -> ThrowsError LispVal
invertBool (Bool val) = return . Bool . not $ val
invertBool notBool = throwError $ TypeMismatch "boolean" notBool