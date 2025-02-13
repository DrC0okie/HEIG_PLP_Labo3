-- Arguments parsing, calling specific operations from external modules, printing usage
-- Authors: Samuel Roland and Timothée Van Hove
module Kit where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad
import Data.Either (isLeft)
import Extract
import JSON
import Merge
import Minify
import Parse
import Pretty
import Sort
import System.Environment
import System.IO

-- JSON Kit entry point
kit :: [String] -> IO ()
kit [] = usage
kit args = case args of
    ("-pretty" : file : _) -> processFile file pretty
    ("-minify" : file : _) -> processFile file minify
    ("-sort" : file : _) -> processFile file (pretty . sort)
    ("-merge" : files) -> processMerge files
    ("-extract" : file : path : _) -> processExtract file path
    unsupported -> do
        putStrLn $ "Error: no action named '" ++ unwords unsupported ++ "'"
        usage

-- Show CLI usage/help
usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $
        "Usage: "
            ++ progName
            ++ " <operation>\
               \\nOperations:\
               \\n  -pretty  <file>                     | Format a JSON file in a human-readable way.\
               \\n  -minify  <file>                     | Remove all unnecessary whitespace from a JSON file.\
               \\n  -extract <file> <path>              | Extract a specific value from a JSON file using a path expression.\
               \\n  -merge   <file> <file>...           | Merge two or more JSON files into one.\
               \\n  -sort    <file>                     | Sort the keys in a JSON object alphabetically."

-- Show error messages
showError :: String -> IO ()
showError err = putStrLn $ "Error: " ++ err

-- General logic to process a single JSON file
processFile :: FilePath -> (JSON -> String) -> IO ()
processFile file operation = do
    content <- readFile file
    case parseJSON content of
        Left err -> showError err
        Right json -> putStrLn $ operation json

-- Special logic for merging multiple files
processMerge :: [FilePath] -> IO ()
processMerge files = do
    when (length files < 2) (showError "Usage: -merge <file1> <file2>...")
    contents <- mapM readFile files
    case traverse parseJSON contents of
        Left err -> showError err
        Right jsons ->
            case foldM merge (head jsons) (tail jsons) of
                Left mergeErr -> showError mergeErr
                Right merged -> print merged

-- Special logic for extracting values
processExtract :: FilePath -> String -> IO ()
processExtract file path = do
    content <- readFile file
    case parseJSON content of
        Left err -> showError err
        Right json -> handleExtract json path

-- Handle extract operation safely
handleExtract :: JSON -> String -> IO ()
handleExtract json path = case extract json path of
    Right result -> print result
    Left e -> showError $ "Error during extraction: " ++ show e
