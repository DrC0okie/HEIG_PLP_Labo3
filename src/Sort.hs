-- Logic for sorting keys of a JSON document
-- Authors: Samuel Roland and Timothée Van Hove

module Sort where

import JSON
import Data.List (sortBy)
import Data.Ord (comparing)
import Pretty

-- Sort a JSON object
sort :: JSON -> JSON
sort (JStr str) = JStr str
sort (JNum num) = JNum num
sort (JBool b) = JBool b
sort JNull = JNull
sort (JObj kvs) = JObj (sortAndRecurse kvs)
sort (JArr elements) = JArr (map sort elements)

-- Helper to sort and recurse on key-value pairs
sortAndRecurse :: [(String, JSON)] -> [(String, JSON)]
sortAndRecurse kvs = 
    let sortedKVs = sortBy (comparing fst) kvs
    in [(k, sort v) | (k, v) <- sortedKVs]