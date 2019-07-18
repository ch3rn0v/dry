module Lib
    ( analyseSourceCode
    )
where

getDirectoryPath :: [String] -> String
getDirectoryPath [] =
    error
        "ERROR:\nThe path to the source code directory was not provided.\
        \ Please provide it as the first argument.\n"
getDirectoryPath (path:_) = path

analyseSourceCode :: [String] -> IO ()
analyseSourceCode args =
    putStrLn $ "\nSource Code Directory is: `" ++ getDirectoryPath args ++ "`"
