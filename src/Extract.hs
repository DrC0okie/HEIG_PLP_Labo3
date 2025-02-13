-- Logic for extracting data in a JSON document with a given path
-- Authors: Samuel Roland and Timothée Van Hove
--
-- Implementation choices
-- "$" cannot have further chars "$.object" is invalid
-- If the path is not valid, it will return an error (out of bounds on array, key not found on object)

module Extract where

import Data.Char
import Data.List
import JSON

data PathToken = Root | Key String | ArrayAccess Int
    deriving (Show, Eq)

-- Split the string at the next separator (. or [ or ])
spanUntilSeparator :: String -> (String, String)
spanUntilSeparator = span (\x -> x /= '.' && x /= '[' && x /= ']')

-- Safe Int reader
safeReadInt :: String -> Either String Int
safeReadInt "" = Left "Error in path: Impossible to parse empty value as integer"
safeReadInt xs = if all isDigit xs then Right (read xs :: Int) else Left $ "Error in path: Non digits char in number " ++ xs

-- Build a list of PathToken to validate the path syntax and use an easier representation
pathTokenize :: String -> Either String [PathToken]
pathTokenize "" = Left "Invalid path, cannot be empty"
pathTokenize "$" = Right [Root]
pathTokenize xs = pathTokenize' xs
  where
    pathTokenize' :: String -> Either String [PathToken]
    pathTokenize' "" = Right []
    pathTokenize' "." = Left "Invalid path, missing key after dot"
    pathTokenize' "]" = Right []
    pathTokenize' (']' : rest) = pathTokenize' rest
    pathTokenize' "[" = Left "Invalid path, invalid array pattern"
    pathTokenize' ys =
        let (f, s) = spanUntilSeparator (tail ys) -- ".a.bc" -> "a.bc" -> ("a", ".bc")
         in case (f, s, ys) of
                -- Key 3 cases
                ("", _, '.' : zs) -> Left $ "Invalid path, missing key after dot in " ++ ys
                (_, "", '.' : zs) -> Right [Key f] -- end of path
                (_, _, '.' : zs) -> pathTokenize' s >>= \ys -> Right $ Key f : ys
                -- ArrayAccess 3 cases - "[2]" -> spanUntilSeparator (tail "[2]) will return ("2", "]")
                ("", _, '[' : zs) -> Left $ "Invalid path, invalid array pattern in " ++ show ys
                (_, "]", '[' : zs) -> safeReadInt f >>= \y -> Right [ArrayAccess y]
                (_, _, '[' : zs) -> safeReadInt f >> (pathTokenize' s >>= \ys -> Right $ ArrayAccess (read f :: Int) : ys)
                (_, _, zs) -> Left $ "Strange path, invalid path pattern at '" ++ ys ++ "'"

extract :: JSON -> String -> Either String JSON
extract json path = do
    tokens <- pathTokenize path -- Step 1: Tokenize the path
    extractWithTokens json tokens -- Step 2: Process the tokens

-- Helper function to process tokens recursively
extractWithTokens :: JSON -> [PathToken] -> Either String JSON
extractWithTokens json [] = Right json -- Base case: No more tokens to process
extractWithTokens json (Root : tokens) = Right json -- Root token, continue processing
extractWithTokens (JObj nodes) (Key key : tokens) =
    case lookup key nodes of
        Nothing -> Left $ "Key not found: " ++ key
        Just value -> extractWithTokens value tokens
extractWithTokens (JArr elements) (ArrayAccess index : tokens) =
    if index < 0 || index >= length elements
        then Left $ "Index out of bounds: " ++ show index
        else extractWithTokens (elements !! index) tokens
extractWithTokens _ _ = Left "Invalid path or incompatible JSON type for path"
