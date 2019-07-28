module Lib
    ( analyseSourceCode
    )
where

import           FileProcessor                  ( readSourceFiles )
import           JSASTProcessor                 ( parseRawSourceFiles )
import           Analyser                       ( analyseParsedSourceFiles
                                                , FunctionPairCompoundSimilarity
                                                )

analyseSourceCode
    :: FilePath -> String -> [String] -> IO [FunctionPairCompoundSimilarity]
analyseSourceCode path ext dirsToSkip =
    analyseParsedSourceFiles
        .   parseRawSourceFiles
        <$> readSourceFiles path ext dirsToSkip

{-
    TODO:
    - move axes weights from `estimateRawFunctionSimilarity` to `aggregateNormalizedDiffs`
    - devise a metric to compare function vectors (consider cosine similarity, or k-NN)

-}
