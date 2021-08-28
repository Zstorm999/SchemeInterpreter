module Parser (
    parseExpr
) where 

import Control.Monad
import Numeric (readOct, readHex)
import Text.ParserCombinators.Parsec hiding (spaces)
import Values

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseChar
        <|> parseBool
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnquote
        <|> parseList


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser LispVal
parseList = do
    char '('
    x <- try parseListElts <|> parseDottedElts
    char ')'
    return x
    where
        parseListElts = liftM List $ sepBy parseExpr spaces
        parseDottedElts = do
            head <- endBy parseExpr spaces
            tail <- char '.' >> spaces >>parseExpr
            return $ DottedList head tail

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return . Atom $ first : rest

readBinary :: String -> Integer
readBinary = readBinaryRec 0
    where
        readBinaryRec sum "" = sum
        readBinaryRec sum (x:xs) = readBinaryRec newSum xs
            where
                newSum = 2 * sum + (if x == '0' then 0 else 1)


parseBinary :: Parser LispVal
parseBinary = do
    try . string $ "#b"
    x <- many1 (oneOf "01")
    return . Number . readBinary $ x

readOctal :: String -> Integer
readOctal = fst . head . readOct

parseOctal :: Parser LispVal
parseOctal = do
    try . string $ "#o"
    x <- many1 octDigit
    return . Number . readOctal $ x

parseDecimal :: Parser LispVal
parseDecimal = parseWithPrefix <|> parseAlone where
        parseWithPrefix = try (string "#d") >> parseAlone

        parseAlone = many1 digit >>= return . Number . read

readHexadecimal :: String -> Integer
readHexadecimal = fst . head . readHex

parseHex :: Parser LispVal
parseHex = do
    try . string $ "#x"
    x <- many1 hexDigit
    return . Number . readHexadecimal $ x


parseNumber :: Parser LispVal
parseNumber = parseOctal
          <|> parseDecimal
          <|> parseHex
          <|> parseBinary

namedChars :: Parser Char
namedChars = do
    x <- try (string "space" <|> string "newline")
    return $ case x of
        "space" -> ' '
        "newline" -> '\n'

parseChar :: Parser LispVal
parseChar = try $ string "#\\" >> (namedChars <|> anyChar) >>= return .Character

escapedChars :: Parser String
escapedChars = do
    char '\\'
    x <- oneOf ['\\', '"', 'n', 't', 'r']
    return $ case x of
        'n' -> "\n"
        't' -> "\t"
        'r' -> "\r"
        _   -> [x]

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ many1 (noneOf "\\\"")
            <|>  escapedChars
    char '"'
    return $ String (concat x)

parseBool :: Parser LispVal
parseBool = do
    x <- try (char '#' >> oneOf "tf")
    return $ case x of
        't' -> Bool True
        'f' -> Bool False

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

