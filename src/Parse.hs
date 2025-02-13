-- Logic for parsing a JSON string into the JSON data types (described in JSON.hs)
-- Authors: Samuel Roland and Timothée Van Hove
module Parse where

import Data.Char (isDigit, isSpace)
import JSON

type ParserResult = Either String (JSON, String)

trim :: String -> String
trim "" = ""
trim xs = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace xs

-- Parse JSON entry point
parseJSON :: String -> Either String JSON
parseJSON s = case trim s of
    "" -> Left "Input is empty"
    input -> case parseValue input of
        Right (json, "") -> Right json
        Right (_, rest) -> Left $ "Unexpected trailing input: " ++ rest
        Left err -> Left err

-- Parse any JSON value
parseValue :: String -> ParserResult
parseValue ('"' : xs) = parseString xs
parseValue ('{' : xs) = parseDelimited parsePair '}' JObj xs
parseValue ('[' : xs) = parseDelimited parseValue ']' JArr xs
parseValue ('t' : 'r' : 'u' : 'e' : xs) = Right (JBool True, xs)
parseValue ('f' : 'a' : 'l' : 's' : 'e' : xs) = Right (JBool False, xs)
parseValue ('n' : 'u' : 'l' : 'l' : xs) = Right (JNull, xs)
parseValue xs@(c : _) | isDigit c || c == '-' = parseNumber xs
parseValue [] = Left "Unexpected end of input while parsing a value"
parseValue xs = Left $ "Unexpected character '" ++ take 10 xs ++ "' while parsing a value"

-- Parse a JSON string
parseString :: String -> ParserResult
parseString xs = case span (/= '"') xs of
    (str, '"' : rest) -> Right (JStr str, rest)
    _ -> Left "Unterminated string: missing closing quote"

-- Parse a JSON number
parseNumber :: String -> ParserResult
parseNumber xs =
    let (numStr, rest) = span (\c -> isDigit c || c == '.' || c == '-') xs
     in if '.' `elem` numStr
            then case reads numStr :: [(Double, String)] of
                [(num, "")] -> Right (JNum (JDouble num), rest)
                _ -> Left $ "Invalid double format near: " ++ take 10 xs
            else case reads numStr :: [(Int, String)] of
                [(num, "")] -> Right (JNum (JInt num), rest)
                _ -> Left $ "Invalid integer format near: " ++ take 10 xs

-- Parse a JSON object or array
parseDelimited :: (String -> Either String (a, String)) -> Char -> ([a] -> JSON) -> String -> ParserResult
parseDelimited elementParser endChar constructor xs = case skipWhitespace xs of
    (c : rest) | c == endChar -> Right (constructor [], rest) -- Empty structure
    rest -> do
        (elements, finalRest) <- parseCommaSeparated elementParser endChar rest
        Right (constructor elements, finalRest)

-- Parse comma-separated values inside arrays or objects
parseCommaSeparated :: (String -> Either String (a, String)) -> Char -> String -> Either String ([a], String)
parseCommaSeparated _ end [] = Left $ "Unexpected end of input: missing closing '" ++ [end] ++ "'" -- Missing closing delimiter
parseCommaSeparated _ end (c : rest) | c == end = Right ([], rest) -- Empty list
parseCommaSeparated parser end xs = do
    (item, rest) <- parser (skipWhitespace xs)
    let rest' = skipWhitespace rest
    case rest' of
        [] -> Left $ "Unexpected end of input: missing closing '" ++ [end] ++ "'" -- Input ends prematurely
        (',' : rest'') -> do
            let rest''Trimmed = skipWhitespace rest''
            case rest''Trimmed of
                (c : _) | c == end -> Left $ "Unexpected trailing comma before '" ++ [end] ++ "'" -- Trailing comma
                _ -> do
                    (more, finalRest) <- parseCommaSeparated parser end rest''
                    Right (item : more, finalRest)
        (c : rest'') | c == end -> Right ([item], rest'') -- End of list
        _ -> Left $ "Unexpected character in list near: " ++ take 10 rest'

-- Parse key-value pairs for objects
parsePair :: String -> Either String ((String, JSON), String)
parsePair xs = case skipWhitespace xs of
    ('"' : rest) ->
        let (key, rest1) = span (/= '"') rest
         in case skipWhitespace (tail rest1) of
                (':' : rest2) -> do
                    (value, rest3) <- parseValue (skipWhitespace rest2)
                    Right ((key, value), rest3)
                _ -> Left $ "Expected ':' after key, but found: " ++ take 10 (skipWhitespace (tail rest1))
    _ -> Left $ "Expected '\"' at the start of a key, but found: " ++ take 10 xs

-- Helper to skip leading whitespace
skipWhitespace :: String -> String
skipWhitespace = dropWhile isSpace
