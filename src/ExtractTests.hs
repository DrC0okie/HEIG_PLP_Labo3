-- Testing logic of Extract.hs functions
-- Authors: Samuel Roland and Timothée Van Hove

module ExtractTests where

import Extract
import JSON
import Test.HUnit
import TestsUtils

tests :: Test
tests =
    TestList
        [ eq
            "Can tokenize path with key"
            (Right [Key "key"])
            $ pathTokenize ".key"
        , eq
            "Can tokenize path with more keys"
            (Right [Key "a", Key "b", Key "b", Key "c", Key "yooo"])
            $ pathTokenize ".a.b.b.c.yooo"
        , eq
            "Can tokenize path with root"
            (Right [Root])
            $ pathTokenize "$"
        , eq
            "Can tokenize path with array access"
            (Right [ArrayAccess 2])
            $ pathTokenize "[2]"
        , eq
            "Can tokenize path with successive array access"
            (Right [ArrayAccess 2, ArrayAccess 6])
            $ pathTokenize "[2][6]"
        , eq
            "Can tokenize path with deep key + array access"
            (Right [Key "cat", Key "legs", ArrayAccess 3, Key "state", ArrayAccess 1])
            $ pathTokenize ".cat.legs[3].state[1]"
        , doesFail
            "Fail to tokenize on empty path"
            (pathTokenize "")
            "cannot be empty"
        , doesFail
            "Fail to tokenize dot"
            (pathTokenize ".")
            "missing key"
        , doesFail
            "Fail to tokenize without key after dot"
            (pathTokenize ".key.")
            "missing key"
        , doesFail
            "Fail to tokenize without key after dot but array access"
            (pathTokenize ".key.[2]")
            "missing key"
        , doesFail
            "Fail to tokenize without dot"
            (pathTokenize "blabla")
            "invalid path pattern"
        , doesFail
            "Fail to tokenize empty array access"
            (pathTokenize ".array[]")
            "invalid array pattern"
        , doesFail
            "Fail to tokenize non integer array access"
            (pathTokenize ".array[yoo]")
            "Non digits char in number"
        , doesFail
            "Fail to tokenize non integer array access 2"
            (pathTokenize ".array[23yoo]")
            "Non digits char in number"
        , eq
            "Can extract root node with $"
            (Right simpleJsonAsJSON)
            $ extract simpleJsonAsJSON "$"
        , eq
            "Can extract first level string"
            (Right $ JStr "Hello world")
            $ extract simpleJsonAsJSON ".string"
        , eq
            "Can extract first level null"
            (Right JNull)
            $ extract simpleJsonAsJSON ".null"
        , eq
            "Can extract first level bool"
            (Right $ JBool True)
            $ extract simpleJsonAsJSON ".boolean"
        , eq
            "Can extract first level number"
            (Right $ JNum (JInt 42))
            $ extract simpleJsonAsJSON ".number"
        , eq
            "Can extract first level array"
            (Right $ JArr [JNum (JInt 1), JNum (JInt 2), JNum (JDouble 3.14)])
            $ extract simpleJsonAsJSON ".array"
        , eq
            "Can extract first level array"
            (Right $ JObj [("key", JStr "value")])
            $ extract simpleJsonAsJSON ".object"
        , eq
            "Can extract second level value inside object"
            (Right $ JStr "value")
            $ extract simpleJsonAsJSON ".object.key"
        , eq
            "Can extract second level inside an array - first key"
            (Right $ JNum (JInt 1))
            $ extract simpleJsonAsJSON ".array[0]"
        , eq
            "Can extract second level inside an array - second key"
            (Right $ JNum (JInt 2))
            $ extract simpleJsonAsJSON ".array[1]"
        , eq
            "Can extract second level inside an array - third key"
            (Right $ JNum (JDouble 3.14))
            $ extract simpleJsonAsJSON ".array[2]"
        , doesFail
            "Extract report tokenize errors"
            (extract simpleJsonAsJSON ".array[23yoo]")
            "Non digits char in number"
        , doesFail
            "Extract detect invalid path"
            (extract simpleJsonAsJSON ".blabla")
            "Key not found"
        , doesFail
            "Extract detect invalid path"
            (extract simpleJsonAsJSON "[3]")
            "Invalid path"
        , doesFail
            "Extract detect invalid path"
            (extract simpleJsonAsJSON ".array[3]")
            "out of bounds"
        , doesFail
            "Extract detect invalid path"
            (extract simpleJsonAsJSON ".array[-3]")
            "Error"
        , doesFail
            "Extract detect invalid path"
            (extract simpleJsonAsJSON "$.object")
            "invalid"
        ]

main :: IO Counts
main = runTestTT tests
