{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment             ( getArgs )
import           Lib                            ( analyseJSONAst )

main :: IO ()
main = do
    args <- getArgs
    if length args < 2
        then
            error
                "\n\nERROR:\nThe path to the json file or the path to the output\
                \ file was not provided. Please provide it as the first argument.\n"
        else
            let pathToRead  = head args
                pathToWrite = args !! 1
            in  do
                    analyseJSONAst pathToRead
                    putStrLn "\nDone."
