-- Testing logic of Minify.hs functions
-- Authors: Samuel Roland and Timothée Van Hove

import JSON
import Minify
import Test.HUnit
import TestsUtils

tests :: Test
tests =
    TestList
        [ eq "Minifies string" "bla  bla" (minify $ JStr "bla  bla")
        , eq "Minifies number" "235234" (minify $ JNum 235234) -- TODO: quid of double and int ?
        , eq "Minifies very simple object" "{\"key\":\"value\"}" (minify $ JObj [("key", JStr "value")])
        , eq "Minifies keys and values with spaces simple object" "{\"key with space\":\"value with    tabs and spa    c e s\",\"   so cool  \":2392323}" (minify $ JObj [("key with space", JStr "value with    tabs and spa    c e s"), ("   so cool  ", JNum 2392323)])
        , eq "Minifies simple array" "[2,3,4]" (minify $ JArr [JNum 2, JNum 3, JNum 4])
        , eq "Minifies simple array 2 with null and bool" "[null,true,false]" (minify $ JArr [JNull, JBool True, JBool False])
        , eq "Minifies simple array with strange content" "[[[{},[[]]],null]]" (minify $ JArr [JArr [JArr [JObj [], JArr [JArr []]], JNull]])
        ]

main :: IO Counts
main = runTestTT tests
