module Lib
    ( getDirectoryPath
    , analyseSourceCode
    )
where

import System.Directory
import Data.List (sort)

getDirectoryPath :: [String] -> String
getDirectoryPath [] =
    error
        "ERROR:\nThe path to the source code directory was not provided.\
        \ Please provide it as the first argument.\n"
getDirectoryPath (path:_) = path

decorateFilename :: String -> String
decorateFilename fn = "- " ++ fn ++ "\n"

printDirectoryContents :: String -> [String] -> IO ()
printDirectoryContents path s =
    putStrLn $ "\nListing Contents of the Directory\n`"
        ++ path
        ++ "`:\n"
        ++ concatMap decorateFilename (reverse s)

analyseSourceCode :: String -> IO ()
analyseSourceCode path =
    listDirectory path >>= printDirectoryContents path

{-
    TODO:
    - Traverse directories recursively
    - Skip ignored directories
    - Build a list of paths to all the source code files
      that were met during traversal
-}
