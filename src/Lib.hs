module Lib
    ( analyseSourceCode
    )
where

import           FileProcessor                  ( readSourceFiles )

analyseSourceCode :: FilePath -> String -> [String] -> IO ()
analyseSourceCode path ext dirsToSkip = do
    filesContents <- readSourceFiles path ext dirsToSkip
    print filesContents

{-
    TODO:
    - Consider parsing and analysing syntactic-level data instead of AST.
      Such as the number of arguments, return values (or lack thereof), potential
      side effects, the identifiers of others functions being called.

-}
