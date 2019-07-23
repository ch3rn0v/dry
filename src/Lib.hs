module Lib
    ( analyseSourceCode
    )
where

import           Data.List                      ( foldl'
                                                , isInfixOf
                                                )
import           Path                           ( parseAbsDir
                                                , fromAbsFile
                                                , Path
                                                , Abs
                                                , File
                                                )
import           Path.IO                        ( listDirRecur )
import           Helpers                        ( mapListOfFunctions )

dirsAbsentInPath :: FilePath -> [String] -> Bool
dirsAbsentInPath path =
    foldl' (\dirsAbsent dir -> dirsAbsent && not (dir `isInfixOf` path)) True

filterFilesByExt :: String -> [FilePath] -> [FilePath]
filterFilesByExt ext = filter
    (\fName ->
        length fName
            >  length ext
            && ext
            == drop (length fName - length ext) fName
    )

filterFilesByDir :: [String] -> [FilePath] -> [FilePath]
filterFilesByDir dirsToSkip = filter (`dirsAbsentInPath` dirsToSkip)

filterSourceCodeFiles
    :: [[FilePath] -> [FilePath]] -> [Path Abs File] -> [FilePath]
filterSourceCodeFiles filters = mapListOfFunctions filters . map fromAbsFile

analyseSourceCode :: FilePath -> [String] -> IO ()
analyseSourceCode path dirsToSkip = do
    dir     <- parseAbsDir path
    (_, fs) <- listDirRecur dir
    let ext             = ".js"
        extFilterFn     = filterFilesByExt ext
        dirFilterFn     = filterFilesByDir dirsToSkip
        sourceCodeFiles = filterSourceCodeFiles [extFilterFn, dirFilterFn] fs
    filesContents <- mapM readFile sourceCodeFiles
    print filesContents

{-
    TODO:
    - Consider parsing and analysing syntactic level data instead of AST.
      Such as the number of arguments, return values (or lack thereof), potential
      side effects, the identifiers of others functions being called.

-}
