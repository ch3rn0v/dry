module Lib
    ( analyseSourceCode
    )
where

import           Data.List                      ( isInfixOf )
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

dirsAbsentInPath :: FilePath -> [String] -> Bool
dirsAbsentInPath path =
    foldl (\dirsAbsent dir -> dirsAbsent && not (dir `isInfixOf` path)) True

filterSourceCodeFiles :: String -> [String] -> [FilePath] -> [FilePath]
filterSourceCodeFiles ext dirsToSkip = filter
    (\f ->
        length f
            >  length ext
            && ext
            == drop (length f - length ext) f
            && dirsAbsentInPath f dirsToSkip
    )

analyseSourceCode :: FilePath -> [String] -> IO ()
analyseSourceCode path dirsToSkip = do
    dir     <- parseAbsDir path
    (_, fs) <- listDirRecur dir
    let ext = ".js"
        sourceCodeFiles =
            filterSourceCodeFiles ext dirsToSkip $ map fromAbsFile fs
    printDirectoryContents path sourceCodeFiles


{-
    TODO:    
    - Read files' contents
    - Parse files' contents (preferably keeping track of the paths)

-}
