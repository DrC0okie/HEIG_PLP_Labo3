{-|
Logic used to merge 2 JSON objects together

Authors: Samuel Roland and Timothée Van Hove

Merge Behavior:

1. Primitive Types:
   - When merging two primitive values (e.g., strings, numbers, booleans, or null), the second value always overwrites the first.

2. Objects (JObj):
   - When merging two objects, keys in the second object overwrite keys in the first object.
   - If a key exists in both objects:
     - If the values associated with the key are compatible, they are merged recursively.
     - If the values are incompatible, an error is returned.
   - Keys that exist only in one object are included in the final merged object.

3. Arrays (JArr):
   - Arrays are concatenated by appending all elements of the second array to the first.

4. Error Handling:
   - If two values are incompatible (e.g., an object and an array), the merge function returns an error.

5. Deeply Nested Structures:
   - The merge function works recursively to merge deeply nested objects and arrays.
   - Conflicts at any level are resolved based on the rules above.

Usage:

let json1 = JObj [("key1", JStr "value1")]
let json2 = JObj [("key1", JStr "value2"), ("key2", JNum (JInt 42))]
let result = merge json1 json2

The `result` will be:
Right (JObj [("key1", JStr "value2"), ("key2", JNum (JInt 42))])

-}


module Merge where

import JSON
import Pretty
import Data.Either

-- Check if two JSON values are compatible for merging
canMerge :: JSON -> JSON -> Either String Bool
canMerge (JStr _) (JStr _) = Right True
canMerge (JNum _) (JNum _) = Right True
canMerge (JBool _) (JBool _) = Right True
canMerge JNull JNull = Right True
canMerge (JObj kvs1) (JObj kvs2) = canMergeObjects kvs1 kvs2
canMerge (JArr _) (JArr _) = Right True
canMerge json1 json2 = Left $ "Cannot merge incompatible types: " ++ show json1 ++ " and " ++ show json2

-- Merge two JSON values
merge :: JSON -> JSON -> Either String JSON
merge json1 json2 = do
    isCompatible <- canMerge json1 json2
    if not isCompatible
        then Left "JSON values are not compatible for merging."
        else case (json1, json2) of
            (JStr _, JStr _) -> Right json2 -- Overwrite
            (JNum _, JNum _) -> Right json2 -- Overwrite
            (JBool _, JBool _) -> Right json2 -- Overwrite
            (JNull, JNull) -> Right JNull
            (JObj kvs1, JObj kvs2) -> JObj <$> mergeObjects kvs1 kvs2
            (JArr arr1, JArr arr2) -> Right $ JArr (arr1 ++ arr2) -- Concatenate arrays
            _ -> Left "Unhandled case during merge."

-- Helper: Check if two JObj key-value pairs are compatible
canMergeObjects :: [(String, JSON)] -> [(String, JSON)] -> Either String Bool
canMergeObjects [] _ = Right True
canMergeObjects ((k1, v1) : rest1) kvs2 =
    case lookup k1 kvs2 of
        Nothing -> canMergeObjects rest1 kvs2 -- No conflict, continue checking
        Just v2 -> do
            compatible <- canMerge v1 v2
            if compatible
                then canMergeObjects rest1 kvs2
                else Left $ "Key conflict for key '" ++ k1 ++ "': " ++ show v1 ++ " and " ++ show v2

-- Helper: Merge two JObj key-value pairs
mergeObjects :: [(String, JSON)] -> [(String, JSON)] -> Either String [(String, JSON)]
mergeObjects [] kvs2 = Right kvs2
mergeObjects ((k1, v1) : rest1) kvs2 =
    case lookup k1 kvs2 of
        Nothing -> do
            mergedRest <- mergeObjects rest1 kvs2
            Right $ (k1, v1) : mergedRest -- Add unique key from kvs1
        Just v2 -> do
            mergedValue <- merge v1 v2 -- Recursively merge overlapping keys
            mergedRest <- mergeObjects rest1 (removeKey k1 kvs2)
            Right $ (k1, mergedValue) : mergedRest

-- Helper: Remove a key from a list of key-value pairs
removeKey :: String -> [(String, JSON)] -> [(String, JSON)]
removeKey key = filter (\(k, _) -> k /= key)


