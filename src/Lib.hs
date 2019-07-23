module Lib
    ( analyseSourceCode
    )
where

import           FileProcessing                 ( getFilesContents )

analyseSourceCode :: FilePath -> String -> [String] -> IO ()
analyseSourceCode path ext dirsToSkip = do
    filesContents <- getFilesContents path ext dirsToSkip
    print filesContents

{-
    TODO:
    - Create a new type that has data about filepath and its contents.
    - Consider parsing and analysing syntactic-level data instead of AST.
      Such as the number of arguments, return values (or lack thereof), potential
      side effects, the identifiers of others functions being called.

-}
