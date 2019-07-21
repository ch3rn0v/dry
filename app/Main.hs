module Main where

import           Lib
import           System.Environment
import           Data.List

main :: IO ()
main = do
    args <- getArgs
    let path       = head args
        dirsToSkip = tail args
    analyseSourceCode path dirsToSkip
