module FileProcessor
    ( readSourceFiles
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
import           Analyser                       ( SourceFile(SourceFile) )

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

-- | Read a file given its path, return its contents along with its path.
readSourceFile :: FilePath -> IO SourceFile
readSourceFile filePath = SourceFile filePath <$> readFile filePath

-- | Recursively traverse every directory within `path`, except `dirsToSkip`,
-- | reading every file's contents as long as its extention is the same as `ext`.
readSourceFiles :: String -> String -> [String] -> IO [SourceFile]
readSourceFiles path ext dirsToSkip = do
    dir     <- parseAbsDir path
    (_, fs) <- listDirRecur dir
    let extFilterFn = filterFilesByExt ext
        dirFilterFn = filterFilesByDir dirsToSkip
        sourceCodeFilePaths =
            filterSourceCodeFiles [extFilterFn, dirFilterFn]
                $ map fromAbsFile fs
    mapM readSourceFile sourceCodeFilePaths
