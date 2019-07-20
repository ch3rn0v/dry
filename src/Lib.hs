module Lib
    ( analyseSourceCode
    )
where

import           Path                           ( parseAbsDir
                                                , fromAbsFile
                                                )
import           Path.IO                        ( listDirRecur )

decoratePathName :: FilePath -> String
decoratePathName fn = "- " ++ fn ++ "\n"

printDirectoryContents :: FilePath -> [FilePath] -> IO ()
printDirectoryContents path files =
    putStrLn
        $  "\nDirectory: `"
        ++ path
        ++ "\nFiles:\n"
        ++ concatMap decoratePathName files

filterSourceCodeFiles :: String -> [FilePath] -> [FilePath]
filterSourceCodeFiles ext = filter
    (\f -> length f > length ext && ext == drop (length f - length ext) f)

analyseSourceCode :: FilePath -> IO ()
analyseSourceCode path = do
    dir     <- parseAbsDir path
    (_, fs) <- listDirRecur dir
    let ext             = ".js"
        sourceCodeFiles = filterSourceCodeFiles ext $ map fromAbsFile fs
    printDirectoryContents path sourceCodeFiles

{-
    TODO:
    - Skip ignored directories
    - Read files' contents

-}
