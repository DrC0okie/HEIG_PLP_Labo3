-- Logic for pretty printing of a JSON document
-- Authors: Samuel Roland and Timothée Van Hove
module Pretty where

import JSON

quote :: Char
quote = '\"'

indentLength :: Int
indentLength = 4

indentChar :: Char
indentChar = ' '

instance Show JSON where
    show :: JSON -> String
    show = pretty

-- Entry point for pretty-printing
pretty :: JSON -> String
pretty = pPrint 0

-- Pretty print with indentation
pPrint :: Int -> JSON -> String
pPrint iLevel (JStr str) = quote : escStr str ++ [quote]
pPrint iLevel (JNum (JInt n)) = show n
pPrint iLevel (JNum (JDouble n)) = show n
pPrint iLevel (JBool True) = "true"
pPrint iLevel (JBool False) = "false"
pPrint iLevel JNull = "null"
pPrint iLevel (JArr elem) = pBracket iLevel '[' ']' (map (pPrint (iLevel + 1)) elem)
pPrint iLevel (JObj elem) = pBracket iLevel '{' '}' (map (formatKV iLevel) elem)

-- Format a key-value pair
formatKV :: Int -> (String, JSON) -> String
formatKV iLevel (key, value) =
    quote : key ++ "\": " ++ pPrint (iLevel + 1) value
        
-- Format bracketed elements like JObj and JArr
pBracket :: Int -> Char -> Char -> [String] -> String
pBracket iLevel open close elements =
    if null elements
        then open: [close] -- Handle empty object/array
        else concat
            [ [open], "\n"
            , concatMap (\e -> indent (iLevel + 1) ++ e ++ ",\n") (init elements)
            , indent (iLevel + 1) ++ last elements ++ "\n"
            , indent iLevel ++ [close]
            ]


-- Escape special characters in strings
escStr :: String -> String
escStr = concatMap escChar
  where
    escChar '"' = "\\\""
    escChar '\\' = "\\\\"
    escChar c = [c]

-- Generate indentation
indent :: Int -> String
indent level = replicate (level * indentLength) indentChar
