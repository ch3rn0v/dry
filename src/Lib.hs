module Lib
    ( analyseJSONAst
    )
where

import           FileProcessor                  ( readJSONFile )
import           ASTProcessor                   ( parseRawJSONFile )
import           Analyser                       ( CSV
                                                , analyseParsedFunctionData
                                                , functionPairSimilarityDataToCsv
                                                )

analyseJSONAst :: FilePath -> IO (Either String CSV)
analyseJSONAst path = do
    rawJSONFile <- readJSONFile path
    let parsedJSONFile = parseRawJSONFile rawJSONFile
    case parsedJSONFile of
        (Left errorMessage) -> return $ Left errorMessage
        (Right functionData) ->
            return
                $ Right
                $ functionPairSimilarityDataToCsv
                $ analyseParsedFunctionData functionData

{-
    TODO:
    - devise a metric to compare function vectors (consider cosine similarity, or k-NN),
      preferably faster than O(N^2)

-}
