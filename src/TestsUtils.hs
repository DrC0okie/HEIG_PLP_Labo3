module TestsUtils where

import Control.Exception (ErrorCall (ErrorCall), SomeException (SomeException), evaluate, try)
import Control.Monad
import Data.Char (toUpper)
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import GHC.Unicode
import JSON
import Minify
import Pretty
import System.Console.Pretty
import System.Directory (doesFileExist, removeDirectory, removeFile)
import Test.HUnit

-- Constants strings and their JSON equivalent
simpleJson = "{\"string\": \"Hello world\", \"number\": 42, \"object\": { \"key\": \"value\" }, \"array\": [1, 2, 3.14], \"boolean\": true, \"null\": null }"
prettyJson = "{\n\t\"string\": \"Hello world\",\n\t\"number\": 42,\n\t\"object\": { \"key\": \"value\" },\n\t\"array\": [1, 2, 3.14],\n\t\"boolean\": true,\n\t\"null\": null\n}"
simpleJsonAsJSON =
    JObj
        [ ("string", JStr "Hello world")
        , ("number", JNum (JInt 42))
        , ("object", JObj [("key", JStr "value")])
        , ("array", JArr [JNum (JInt 1), JNum (JInt 2), JNum (JDouble 3.14)])
        , ("boolean", JBool True)
        , ("null", JNull)
        ]

-- TODO: more complex JSON tree

trim :: String -> String
trim "" = ""
trim xs = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace xs

outContains :: String -> String -> Assertion
outContains res content = unless (content `isInfixOf` res) (assertFailure (color Yellow "Captured output '" ++ color Cyan (trim res) ++ color Yellow "' should contain " ++ color Cyan (show content)))

assertFileContains :: String -> String -> Assertion
assertFileContains filename expectedContent = do
    doesExist <- doesFileExist filename
    unless doesExist (assertFailure (color Yellow $ "File " ++ filename ++ " should exist but was not found."))
    givenContent <- readFile filename
    unless (expectedContent `isInfixOf` givenContent) (assertFailure (color Yellow "Given error " ++ color Cyan (show expectedContent) ++ color Yellow " should contain " ++ color Cyan (show givenContent)))

jsonFile :: String
jsonFile = "test.json"

debug = True

--
ioTest :: String -> Assertion -> Test
ioTest title assertion =
    TestLabel
        title
        (TestCase assertion)

-- Shortcut for shorter assertions, instead of "TestCase (assertEqual ...)" just "eq ..."
eq :: (Eq a, Show a) => String -> a -> a -> Test
eq title expected given =
    TestCase
        ( do
            unless (expected == given) $
                assertFailure $
                    color Red title
                        ++ color Yellow ": Expected equality with \n"
                        ++ color Cyan (show expected)
                        ++ color Yellow "\nbut got \n"
                        ++ color Cyan (show given)
            when (debug && expected == given) $ putStrLn $ color Green ("\n" ++ title ++ ": Equality matched with " ++ show given)
        )

-- doesFail makes sure the given expression will fail via the error function
-- and contains the expectedError inside the given error (it may be equal)
doesFail :: (Show a) => String -> Either String a -> String -> Test
doesFail title expression expectedError =
    TestCase
        ( do
            -- We use ErrorCall (instead of SomeException) to only catch
            -- exception generated by the "error" function
            result <- evaluate expression
            -- Assert this is an exception, by failing
            when
                (isRight result)
                ( do
                    let Right json = result
                    assertFailure (color Red title ++ color Yellow ": Should gave error '" ++ expectedError ++ "' but returned " ++ show json ++ ".")
                )

            -- Checking the error message content
            let Left actualError = result
            unless
                (expectedError `isInfixOf` actualError)
                (assertFailure (color Red title ++ color Yellow ": Given error " ++ color Cyan (show actualError) ++ color Yellow " failed to contains " ++ color Cyan (show expectedError)))

            when debug $ putStrLn $ color Green ("\nGot expected error " ++ show actualError)
        )
