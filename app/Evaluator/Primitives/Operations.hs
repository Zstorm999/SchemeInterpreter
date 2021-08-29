module Evaluator.Primitives.Operations (
    unOp,
    boolBinop
) where 

import Values


unOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unOp op [x] = op x
unOp op notUnique = throwError $ NumArgs 1 notUnique

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                    then throwError $ NumArgs 2 args
                                    else do
                                        left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return . Bool $ op left  right