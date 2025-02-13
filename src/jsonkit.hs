-- Entry point of the JSON Kit
-- Authors: Samuel Roland and Timothée Van Hove
-- Description: This project is the implementation of JSON utility tool, like jq,
-- that supports a few operations like pretty printing, minification, extraction,
-- merge and keys sorting. The help can be called by calling the CLI without any argument.
--
-- Note:
--    This main is very short because we wanted to write integration tests
--    for CLI arguments, so we created another "main" function kit that takes
--    an array of string, in separate module Kit.hs that can be tested

import Kit
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    kit args
