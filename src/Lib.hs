module Lib
    ( analyseSourceCode
    )
where

import           FileProcessor                  ( readSourceFiles )
import           JSASTProcessor                 ( parseRawSourceFiles )

analyseSourceCode :: FilePath -> String -> [String] -> IO ()
analyseSourceCode path ext dirsToSkip = do
    rawSourceFiles <- readSourceFiles path ext dirsToSkip
    let parsedSourceFiles = parseRawSourceFiles rawSourceFiles
    print parsedSourceFiles

{-
    TODO:
    - add Analyser module with a FunctionData -> FunctionVectorData function
    - devise a metric to compare function vectors (consider cosine similarity, or k-NN)

-}
