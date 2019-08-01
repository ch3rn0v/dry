module Main where

import           Lib                            ( analyseSourceCode )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    if length args < 2
        then
            error
                "\n\nERROR:\nThe path to the source code directory or the path to\
                \ the output file was not provided. Please provide it as the first argument.\n"
        else
            let pathToRead  = head args
                pathToWrite = args !! 1
                dirsToSkip  = drop 2 args
                ext         = ".js"
            in  do
                    analysedSourceCodeCSV <- analyseSourceCode pathToRead
                                                               ext
                                                               dirsToSkip
                    writeFile pathToWrite analysedSourceCodeCSV
                    putStrLn "\nDone."
