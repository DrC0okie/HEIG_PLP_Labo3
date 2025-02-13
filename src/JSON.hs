-- Core data types for JSON intermediate representation
-- Authors: Samuel Roland and Timothée Van Hove

module JSON where

data JNumber = JInt Int | JDouble Double
    deriving (Show, Eq)

data JSON = JStr String | JNum JNumber | JObj [(String, JSON)] | JArr [JSON] | JBool Bool | JNull
    deriving (Eq)

-- Usage examples
--
-- First example
-- {
-- "string": "Hello, World!",
-- "number": 42,
-- "object": { "key": "value" },
-- "array": [1, 2, 3],
-- "boolean": true,
-- "null": null
-- }

-- tryJson :: JSON
-- tryJson = JObj [("string", JStr "Hello world"), ("number", JNum 42), ("object", JObj [("key", JStr "value")]), ("array", JArr [JNum 1, JNum 2, JNum 3])]

-- Second example
-- [
--     [
--         true,
--         null,
--         2001
--     ],
--     {},
--     [],
--     [
--         {
--             "k": [
--                 true
--             ]
--         }
--     ]
-- ]

-- try2Json :: JSON
-- try2Json = JArr [JArr [JBool True, JNull, JNum 2001], JObj [], JArr [], JArr [JObj [("k", JArr [JBool True])]]]
