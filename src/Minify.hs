-- Logic used to minify a JSON document
-- Authors: Samuel Roland and Timothée Van Hove

module Minify where

import Data.List
import JSON

-- Give a minified string version of the JSON structure
-- Here "minified" means the shortest valid string representation,
-- without any extra cosmetic space or tabs
-- Space characters insides values are obviously left untouched.
minify :: JSON -> String
minify (JStr str) = show str
minify (JNum num) = show num
minify (JBool True) = "true"
minify (JBool False) = "false"
minify JNull = "null"
minify (JObj kvs) = "{" ++ intercalate "," (map (\(k, v) -> show k ++ ":" ++ minify v) kvs) ++ "}"
minify (JArr vs) = "[" ++ intercalate "," (map minify vs) ++ "]"
