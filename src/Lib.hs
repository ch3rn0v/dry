module Lib
    ( analyseSourceCode
    )
where

import           Data.List                      ( foldl'
                                                , isInfixOf
                                                )
import           Data.Foldable                  ( traverse_ )
import           Path                           ( parseAbsDir
                                                , fromAbsFile
                                                )
import           Path.IO                        ( listDirRecur )


printFilesContent :: [(FilePath, String)] -> IO ()
printFilesContent = traverse_
    $ \(fp, fc) -> putStrLn $ "File: `" ++ fp ++ "`\nContents:\n" ++ fc

dirsAbsentInPath :: FilePath -> [String] -> Bool
dirsAbsentInPath path =
    foldl' (\dirsAbsent dir -> dirsAbsent && not (dir `isInfixOf` path)) True

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
    filesContents <- mapM readFile sourceCodeFiles
    printFilesContent $ zip sourceCodeFiles filesContents

{-
    TODO:
    - Parse files' contents (preferably keeping track of the paths)

-}
