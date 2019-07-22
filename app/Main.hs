module Main where

import           Lib
import           System.Environment
import           Data.List

main :: IO ()
main = do
    args <- getArgs
    if null args
        then
            error
                "\n\nERROR:\nThe path to the source code directory was not provided.\
                \ Please provide it as the first argument.\n"
        else
            let path       = head args
                dirsToSkip = tail args
            in  analyseSourceCode path dirsToSkip
