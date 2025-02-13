-- This module aggregates all test suites and runs them as a single suite.
-- Authors: Samuel Roland and Timothée Van Hove
-- Note: Ensure the following libraries are installed for the tests to run properly:
--       cabal install --lib pretty-terminal
--       cabal install --lib silently
--       cabal install --lib directory
module AllTests where

import ExtractTests (tests)
import MergeTests (tests)
import ParseTests (tests)
import PrettyTests (tests)
import SortTests (tests)
import Test.HUnit

testsSuites :: Test
testsSuites =
    TestList
        [ ExtractTests.tests
        , MergeTests.tests
        , ParseTests.tests
        , PrettyTests.tests
        , SortTests.tests
        ]

main :: IO Counts
main = runTestTT testsSuites
