module Evaluator.Primitives (
    primitives,
) where

import Values

import Evaluator.Primitives.Operations

import qualified Evaluator.Primitives.Bool as Bool
import qualified Evaluator.Primitives.Char as Char
import qualified Evaluator.Primitives.List as List
import qualified Evaluator.Primitives.Number as Number
import qualified Evaluator.Primitives.String as String
import qualified Evaluator.Primitives.Symbol as Symbol

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = Bool.primitives
          ++ Char.primitives
          ++ List.primitives
          ++ Number.primitives
          ++ String.primitives
          ++ Symbol.primitives
          ++ [("eqv?", eqv),
              ("eq?", eqv)]

eqv :: [LispVal] -> ThrowsError LispVal
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                                    Left err -> False
                                    Right (Bool val) -> val

eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Character  arg1, Character arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList




