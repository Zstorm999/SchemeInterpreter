module Values (
    LispVal(..),
    unwordsList,
    LispError (..),
    ThrowsError,
    trapError,
    extractValue,
    IOThrowsError,
    liftThrows,
    runIOThrows,
    Except.throwError,
    Env
) where

import Data.IORef
import Data.Functor
import Text.Parsec ( ParseError )
import qualified Control.Monad.Except as Except



data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | Character Char
    | String String
    | Bool Bool
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func {params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env }

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Character c) = "#\\" ++ case c of
                                    '\n' -> "newline"
                                    ' ' -> "space"
                                    _    -> [c]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List content) = "(" ++ unwordsList content ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++ (case varargs of
                                                    Nothing -> ""
                                                    Just arg -> " . " ++ arg) ++ ")...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal



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

-- env exception handling

type IOThrowsError = Except.ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = Except.throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = Except.runExceptT (trapError action) <&> extractValue

type Env = IORef [(String, IORef LispVal)]