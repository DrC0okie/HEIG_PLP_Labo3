module ParseTests where

import JSON
import Parse
import Test.HUnit
import TestsUtils

tests :: Test
tests =
    TestList
        [ 
        -- Nominal Cases
        eq
            "simpleJson can be parsed"
            (Right simpleJsonAsJSON)
            (parseJSON simpleJson)
        , eq
            "prettyJson can be parsed too"
            (Right simpleJsonAsJSON)
            (parseJSON prettyJson)

        , eq
            "Object with mixed types can be parsed"
            (Right $ JObj [("key1", JStr "value"), ("key2", JNum (JInt 42))])
            (parseJSON "{\"key1\": \"value\", \"key2\": 42}")
        , eq
            "Empty object can be parsed"
            (Right $ JObj [])
            (parseJSON "{}")
        , eq
            "Array with mixed numbers and boolean can be parsed"
            (Right $ JArr [JNum (JInt 1), JNum (JDouble 3.14), JBool True])
            (parseJSON "[1, 3.14, true]")
        , eq
            "Empty array can be parsed"
            (Right $ JArr [])
            (parseJSON "[]")
        , eq
            "Boolean true can be parsed"
            (Right $ JBool True)
            (parseJSON "true")
        , eq
            "Boolean false can be parsed"
            (Right $ JBool False)
            (parseJSON "false")
        , eq
            "Null value can be parsed"
            (Right JNull)
            (parseJSON "null")
        , eq
            "Integer number can be parsed"
            (Right $ JNum (JInt 123))
            (parseJSON "123")
        , eq
            "Floating-point number can be parsed"
            (Right $ JNum (JDouble 3.14))
            (parseJSON "3.14")
        , eq
            "String value can be parsed"
            (Right $ JStr "Hello, world!")
            (parseJSON "\"Hello, world!\"")

        -- Error Cases
        , doesFail
            "Object missing closing '}'"
            (parseJSON "{\"key\": \"value\"")
            "Unexpected end of input: missing closing '}'"
        , doesFail
            "Object missing closing ']'"
            (parseJSON "[1, 2, 3.14")
            "Unexpected end of input: missing closing ']'"
        , doesFail
            "Object missing colon"
            (parseJSON "{\"key\" \"value\"}")
            "Expected ':' after key"
        , doesFail
            "Array with trailing comma before closing ']'"
            (parseJSON "[1, 2,]")
            "Unexpected trailing comma before ']'"
        , doesFail
            "Object with trailing comma before closing '}'"
            (parseJSON "{\"key\": \"value\",}")
            "Unexpected trailing comma before '}'"
        , doesFail
            "Invalid null value"
            (parseJSON "nul")
            "Unexpected character 'nul'"
        , doesFail
            "Unterminated string"
            (parseJSON "\"This is invalid")
            "Unterminated string: missing closing quote"
        ]

main :: IO Counts
main = runTestTT tests
