module Errors(
    LispError (..),
    ThrowsError,
    trapError,
    extractValue,
    Except.throwError
) where

import Values ( LispVal, unwordsList )
import Text.Parsec ( ParseError )
import qualified Control.Monad.Except as Except

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Unspecified String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " arg" ++ (if expected > 1 then "s" else "") ++ ": found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ show expected ++ ", found " ++ show found
showError (Parser parseError) = "Parse error at: " ++ show parseError
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varName) = message ++ ": " ++ show varName

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError :: (Except.MonadError e m, Show e) => m String -> m String
trapError action = Except.catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val