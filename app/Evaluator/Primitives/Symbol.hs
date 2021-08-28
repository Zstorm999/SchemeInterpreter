module Evaluator.Primitives.Symbol (
    primitives
) where 

import Errors 
import Evaluator.Primitives.Operations 
import Values



primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("symbol?", unOp isSymbol),
    ("symbol->string", unOp symbolToString),
    ("string->symbol", unOp stringToSymbol)]


isSymbol :: LispVal -> ThrowsError LispVal
isSymbol (Atom _) = return $ Bool True
isSymbol _ = return $ Bool False

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom val) = return $ String val
symbolToString notAtom = throwError $ TypeMismatch "symbol" notAtom

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String val) = return $ Atom val
stringToSymbol notString = throwError $ TypeMismatch "string" notString