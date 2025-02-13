-- Testing logic of Kit.hs functions
-- Authors: Samuel Roland and Timothée Van Hove
import Control.Monad
import Data.Char (isSpace)
import Data.List
import Kit
import System.Console.Pretty
import System.IO.Silently
import Test.HUnit
import TestsUtils

-- Return the extracted string from captured output from given IO expression
-- TODO: refactor with bind operator ?
-- Helper to capture stdout and run an IO action
output :: IO () -> IO String
output expr = do
    res <- capture expr
    return $ fst res

-- Test constants
testJsonFile :: FilePath
testJsonFile = "test.json"

tests :: Test
tests =
    TestList
        [ -- Usage
          ioTest
            "Kit shows usage on zero argument"
            ( do
                out <- output $ kit []
                outContains out "Usage: <interactive> <operation>"
                outContains out "Operations"
            )
        , ioTest
            "Kit shows usage for unsupported operation"
            ( do
                out <- output $ kit ["-unsupported"]
                outContains out "Error: no action named '-unsupported'"
            )
          -- Invalid JSON
        , ioTest
            "Error when file contains invalid JSON"
            ( do
                writeFile testJsonFile "{invalid_json}"
                out <- output $ kit ["-pretty", testJsonFile]
                outContains out "Error:"
            )
          -- Argument validation
        , ioTest
            "Merge operation with insufficient arguments"
            ( do
                out <- output $ kit ["-merge", testJsonFile]
                outContains out "Error: Usage: -merge <file1> <file2>..."
            )
        ]

main :: IO Counts
main = runTestTT tests
