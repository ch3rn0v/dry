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
    - write analysis results into a file
    - move axes weights from `estimateRawFunctionSimilarity` to `aggregateNormalizedDiffs`
    - devise a metric to compare function vectors (consider cosine similarity, or k-NN)

-}
