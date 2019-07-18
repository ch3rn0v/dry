module Main where

import           Lib
import           System.Environment
import           Data.List

main :: IO ()
main = do
    args <- getArgs
    analyseSourceCode args
