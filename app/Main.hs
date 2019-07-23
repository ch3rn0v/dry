module Main where

import           Lib                            ( analyseSourceCode )
import           System.Environment             ( getArgs )

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
                ext        = ".js"
                dirsToSkip = tail args
            in  analyseSourceCode path ext dirsToSkip
