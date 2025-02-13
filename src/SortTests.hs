-- Testing logic of Sort.hs functions
-- Authors: Samuel Roland and Timothée Van Hove

module SortTests where

import JSON
import Sort
import Test.HUnit
import TestsUtils

tests :: Test
tests =
    TestList
        [ -- Primitive types should remain unchanged
          eq
            "Sort a string"
            (sort (JStr "Hello"))
            (JStr "Hello")
        , eq
            "Sort a number (integer)"
            (sort (JNum (JInt 42)))
            (JNum (JInt 42))
        , eq
            "Sort a number (double)"
            (sort (JNum (JDouble 3.14)))
            (JNum (JDouble 3.14))
        , eq
            "Sort a boolean (True)"
            (sort (JBool True))
            (JBool True)
        , eq
            "Sort a boolean (False)"
            (sort (JBool False))
            (JBool False)
        , eq
            "Sort a null value"
            (sort JNull)
            JNull

          -- Arrays
        , eq
            "Sort an array of primitives"
            (sort (JArr [JNum (JInt 3), JStr "B", JNum (JInt 1)]))
            (JArr [JNum (JInt 3), JStr "B", JNum (JInt 1)]) -- Arrays are left unchanged
        , eq
            "Sort an array of objects"
            (sort (JArr [JObj [("w", JNum (JInt 2)), ("b", JNum (JDouble 3.14))], JObj [("a", JNum (JDouble 2.71))]]))
            (JArr [JObj [("b", JNum (JDouble 3.14)), ("w", JNum (JInt 2))], JObj [("a", JNum (JDouble 2.71))]])

          -- Objects
        , eq
            "Sort a simple object"
            (sort (JObj [("b", JNum (JInt 2)), ("a", JNum (JInt 1))]))
            (JObj [("a", JNum (JInt 1)), ("b", JNum (JInt 2))])
        , eq
            "Sort an object with nested objects"
            (sort (JObj [("z", JObj [("b", JNum (JInt 2)), ("a", JNum (JInt 1))]), ("a", JStr "start")]))
            (JObj [("a", JStr "start"), ("z", JObj [("a", JNum (JInt 1)), ("b", JNum (JInt 2))])])
        , eq
            "Sort an object with arrays"
            (sort (JObj [("array", JArr [JNum (JInt 3), JNum (JInt 1)]), ("key", JStr "value")]))
            (JObj [("array", JArr [JNum (JInt 3), JNum (JInt 1)]), ("key", JStr "value")])

          -- Edge cases
        , eq
            "Sort an empty object"
            (sort (JObj []))
            (JObj [])
        , eq
            "Sort an empty array"
            (sort (JArr []))
            (JArr [])
        , eq
            "Sort deeply nested structures"
            (sort (JObj [("b", JObj [("d", JNum (JInt 4)), ("c", JNum (JInt 3))]), ("a", JArr [JObj [("y", JStr "z"), ("x", JStr "y")]])]))
            (JObj [("a", JArr [JObj [("x", JStr "y"), ("y", JStr "z")]]), ("b", JObj [("c", JNum (JInt 3)), ("d", JNum (JInt 4))])])
        ]

main :: IO Counts
main = runTestTT tests
