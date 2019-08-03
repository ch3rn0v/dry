module Lib
    ( analyseSourceCode
    )
where

import           FileProcessor                  ( readSourceFiles )
import           JSASTProcessor                 ( parseRawSourceFiles
                                                , partitionASTParsingResults
                                                )
import           Analyser                       ( CSV
                                                , analyseParsedSourceFiles
                                                , functionPairSimilarityDataToCsv
                                                )

analyseSourceCode :: FilePath -> String -> [String] -> IO CSV
analyseSourceCode path ext dirsToSkip = do
    sourceFiles <- readSourceFiles path ext dirsToSkip
    let parsedRawSourceFiles = parseRawSourceFiles sourceFiles
    -- Note that `partitionASTParsingResults` _outputs_ errors (if any),
    -- and returns only correct ASTs
    correctASTs <- partitionASTParsingResults parsedRawSourceFiles
    let analysedASTs = analyseParsedSourceFiles correctASTs
    pure $ functionPairSimilarityDataToCsv analysedASTs

{-
    TODO:
    - introduce weights to Constr Occurrence Maps (see `countConstructorOccurrences`)
    - devise a metric to compare function vectors (consider cosine similarity, or k-NN)

-}
