module FileProcessing
    ( getFilesContents
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

-- | Keep only the files whose path ends in the given extension `ext`.
filterFilesByExt :: String -> [FilePath] -> [FilePath]
filterFilesByExt ext = filter
    (\fName ->
        length fName
            >  length ext
            && ext
            == drop (length fName - length ext) fName
    )

dirsAbsentInPath :: FilePath -> [String] -> Bool
dirsAbsentInPath path =
    foldl' (\dirsAbsent dir -> dirsAbsent && not (dir `isInfixOf` path)) True

-- | Keep only the files whose path doesn't include any of the given directories.
filterFilesByDir :: [String] -> [FilePath] -> [FilePath]
filterFilesByDir dirsToSkip = filter (`dirsAbsentInPath` dirsToSkip)

-- | Keep only the files whose file paths meet the criteria of all the `filters`.
filterSourceCodeFiles :: [[FilePath] -> [FilePath]] -> [FilePath] -> [FilePath]
filterSourceCodeFiles = mapListOfFunctions

-- | Recursively traverse every directory within `path`, except `dirsToSkip`,
-- | reading every file's contents as long as its extention is the same as `ext`.
getFilesContents :: String -> String -> [String] -> IO [String]
getFilesContents path ext dirsToSkip = do
    dir     <- parseAbsDir path
    (_, fs) <- listDirRecur dir
    let extFilterFn = filterFilesByExt ext
        dirFilterFn = filterFilesByDir dirsToSkip
        sourceCodeFiles =
            filterSourceCodeFiles [extFilterFn, dirFilterFn]
                $ map fromAbsFile fs
    mapM readFile sourceCodeFiles
