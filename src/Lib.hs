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
import           Language.JavaScript.Parser     ( parseModule )
import           Language.JavaScript.Parser.AST ( JSAST )

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

printAST :: Either String JSAST -> String
printAST (Left  _  ) = "ERROR: Parsing failed"
printAST (Right ast) = show ast

printFilesContent :: [(FilePath, Either String JSAST)] -> IO ()
printFilesContent = traverse_ $ \(fp, fAST) ->
    putStrLn $ "File: `" ++ fp ++ "`\nAST:\n" ++ printAST fAST

analyseSourceCode :: FilePath -> [String] -> IO ()
analyseSourceCode path dirsToSkip = do
    dir     <- parseAbsDir path
    (_, fs) <- listDirRecur dir
    let ext = ".js"
        sourceCodeFiles =
            filterSourceCodeFiles ext dirsToSkip $ map fromAbsFile fs
    filesContents <- mapM readFile sourceCodeFiles
    let jsASTs = zipWith parseModule filesContents sourceCodeFiles
    printFilesContent $ zip sourceCodeFiles jsASTs

{-
    TODO:
    - Process AST to find similar functions

-}
