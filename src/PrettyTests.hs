-- Testing logic of Pretty.hs functions
-- Authors: Samuel Roland and Timothée Van Hove

module PrettyTests where

import JSON
import Pretty
import Test.HUnit
import TestsUtils

-- Define test cases
tests :: Test
tests =
    TestList
        [ eq
            "Pretty print a string"
            "\"Hello, World!\""
            (pretty $ JStr "Hello, World!")
        , eq
            "Pretty print an integer"
            "42"
            (pretty $ JNum (JInt 42))
        , eq
            "Pretty print a double"
            "3.1415"
            (pretty $ JNum (JDouble 3.1415))
        , eq
            "Pretty print true"
            "true"
            (pretty $ JBool True)
        , eq
            "Pretty print false"
            "false"
            (pretty $ JBool False)
        , eq
            "Pretty print null"
            "null"
            (pretty JNull)
        , eq
            "Pretty print an empty array"
            "[]"
            (pretty $ JArr [])
        , eq
            "Pretty print an empty object"
            "{}"
            (pretty $ JObj [])
        , eq
            "Pretty print an array with simple values"
            "[\n    1,\n    \"string\",\n    true\n]"
            (pretty $ JArr [JNum (JInt 1), JStr "string", JBool True])
        , eq
            "Pretty print a nested array"
            "[\n    1,\n    [\n        2,\n        3\n    ]\n]"
            (pretty $ JArr [JNum (JInt 1), JArr [JNum (JInt 2), JNum (JInt 3)]])
        , eq
            "Pretty print an object with simple key-value pairs"
            "{\n    \"key1\": \"value\",\n    \"key2\": 42\n}"
            (pretty $ JObj [("key1", JStr "value"), ("key2", JNum (JInt 42))])
        , eq
            "Pretty print a nested object"
            "{\n    \"key\": {\n        \"nestedKey\": \"nestedValue\"\n    }\n}"
            (pretty $ JObj [("key", JObj [("nestedKey", JStr "nestedValue")])])
        , eq
            "Pretty print an object with mixed types"
            "{\n    \"string\": \"value\",\n    \"number\": 42,\n    \"boolean\": true,\n    \"array\": [\n        1,\n        2\n    ],\n    \"null\": null\n}"
            (pretty $ JObj
                [ ("string", JStr "value")
                , ("number", JNum (JInt 42))
                , ("boolean", JBool True)
                , ("array", JArr [JNum (JInt 1), JNum (JInt 2)])
                , ("null", JNull)
                ])
        ]

main :: IO Counts
main = runTestTT tests
