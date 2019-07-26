module Lib
    ( analyseSourceCode
    )
where

import           FileProcessor                  ( readSourceFiles )
import           JSASTProcessor                 ( parseRawSourceFiles )
import           Analyser                       ( analyseParsedSourceFiles )

analyseSourceCode :: FilePath -> String -> [String] -> IO ()
analyseSourceCode path ext dirsToSkip = do
    rawSourceFiles <- readSourceFiles path ext dirsToSkip
    let parsedSourceFiles = parseRawSourceFiles rawSourceFiles
        analysisResults   = analyseParsedSourceFiles parsedSourceFiles
    print analysisResults

{-
    TODO:
    - add Analyser module with a FunctionData -> FunctionVectorData function
    - devise a metric to compare function vectors (consider cosine similarity, or k-NN)

-}
