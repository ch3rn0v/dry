module Lib
    ( analyseSourceCode
    )
where

import           FileProcessor                  ( readSourceFiles )
import           JSASTProcessor                 ( parseRawSourceFiles )
import           Analyser                       ( CSV
                                                , analyseParsedSourceFiles
                                                , functionPairSimilarityDataToCsv
                                                )

analyseSourceCode :: FilePath -> String -> [String] -> IO CSV
analyseSourceCode path ext dirsToSkip =
    functionPairSimilarityDataToCsv
        .   analyseParsedSourceFiles
        .   parseRawSourceFiles
        <$> readSourceFiles path ext dirsToSkip

{-
    TODO:
    - move axes weights from `estimateRawFunctionSimilarity` to `aggregateNormalizedDiffs`
    - devise a metric to compare function vectors (consider cosine similarity, or k-NN)

-}
