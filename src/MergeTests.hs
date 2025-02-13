-- Testing logic of Merge.hs functions
-- Authors: Samuel Roland and Timothée Van Hove

module MergeTests where

import JSON
import Merge
import Test.HUnit
import TestsUtils
import Pretty

tests :: Test
tests =
    TestList
        [ -- Primitive types (JStr, JNum, JBool, JNull)
          eq
            "Merge two strings"
            (merge (JStr "hello") (JStr "world"))
            (Right (JStr "world"))
        , eq
            "Merge two integers"
            (merge (JNum (JInt 42)) (JNum (JInt 99)))
            (Right (JNum (JInt 99)))
        , eq
            "Merge two doubles"
            (merge (JNum (JDouble 3.14)) (JNum (JDouble 2.71)))
            (Right (JNum (JDouble 2.71)))
        , eq
            "Merge two booleans"
            (merge (JBool True) (JBool False))
            (Right (JBool False))
        , eq
            "Merge null values"
            (merge JNull JNull)
            (Right JNull)

          -- Arrays
        , eq
            "Merge two arrays"
            (merge (JArr [JNum (JInt 1), JNum (JInt 2)]) (JArr [JNum (JInt 3)]))
            (Right (JArr [JNum (JInt 1), JNum (JInt 2), JNum (JInt 3)]))

          -- Objects
        , eq
            "Merge two disjoint objects"
            (merge (JObj [("key1", JStr "value1")]) (JObj [("key2", JNum (JInt 42))]))
            (Right (JObj [("key1", JStr "value1"), ("key2", JNum (JInt 42))]))
        , eq
            "Merge two overlapping objects (same key)"
            (merge (JObj [("key1", JStr "value1")]) (JObj [("key1", JStr "value2")]))
            (Right (JObj [("key1", JStr "value2")]))

          -- Nested structures
        , eq
            "Merge nested objects"
            (merge
                (JObj [("key1", JObj [("nestedKey1", JStr "value1")])])
                (JObj [("key1", JObj [("nestedKey2", JNum (JInt 42))])])
            )
            (Right (JObj [("key1", JObj [("nestedKey1", JStr "value1"), ("nestedKey2", JNum (JInt 42))])]))
        , eq
            "Merge nested arrays"
            (merge (JArr [JArr [JNum (JInt 1)], JNum (JInt 2)]) (JArr [JArr [JNum (JInt 3)]])) -- [[1], 2] [[3]] = [[1], 2, [3]]
            (Right (JArr [JArr [JNum (JInt 1)], JNum (JInt 2), JArr [JNum (JInt 3)]]))

        , eq
            "Merge complex nested JSON structures"
            (merge
                (JObj
                    [ ("key1", JArr [JObj [("nestedKey1", JStr "value1")], JNum (JInt 2)])
                    , ("key2", JObj [("nestedKey2", JNum (JInt 42))])
                    ]
                )
                (JObj
                    [ ("key1", JArr [JObj [("nestedKey3", JBool True)]])
                    , ("key3", JStr "new value")
                    ]
                )
            )
            (Right
                (JObj
                    [ ("key1", JArr
                        [ JObj [("nestedKey1", JStr "value1")]
                        , JNum (JInt 2)
                        , JObj [("nestedKey3", JBool True)]
                        ]
                      )
                    , ("key2", JObj [("nestedKey2", JNum (JInt 42))])
                    , ("key3", JStr "new value")
                    ]
                )
            )

          -- Edge cases
        , eq
            "Merge empty objects"
            (merge (JObj []) (JObj []))
            (Right (JObj []))
        , eq
            "Merge empty arrays"
            (merge (JArr []) (JArr []))
            (Right (JArr []))
        , eq
            "Merge object with empty object"
            (merge (JObj [("key", JStr "value")]) (JObj []))
            (Right (JObj [("key", JStr "value")]))
        , eq
            "Merge array with empty array"
            (merge (JArr [JNum (JInt 1)]) (JArr []))
            (Right (JArr [JNum (JInt 1)]))

          -- Errors
        , doesFail
            "Cannot merge incompatible types (string and object)"
            (merge (JStr "value") (JObj [("key", JStr "value")]))
            "Cannot merge incompatible types"
        , doesFail
            "Cannot merge incompatible nested objects (conflicting key types)"
            (merge (JObj [("key", JStr "value")]) (JObj [("key", JNum (JInt 42))]))
            "Cannot merge incompatible types: \"value\" and 42"

          -- Deep nesting
        , eq
            "Merge deeply nested objects (3 levels)"
            (merge
                (JObj [("key", JObj [("nested1", JObj [("nested2", JStr "value1")])])])
                (JObj [("key", JObj [("nested1", JObj [("nested2", JStr "value2"), ("nested3", JNum (JInt 42))])])])
            )
            (Right (JObj [("key", JObj [("nested1", JObj [("nested2", JStr "value2"), ("nested3", JNum (JInt 42))])])]))
        , eq
            "Merge deeply nested arrays (3 levels)"
            (merge (JArr [JArr [JArr [JNum (JInt 1)]]]) (JArr [JArr [JArr [JNum (JInt 2)]]]))
            (Right (JArr [JArr [JArr [JNum (JInt 1)]], JArr [JArr [JNum (JInt 2)]]]))
        ]

main :: IO Counts
main = runTestTT tests
