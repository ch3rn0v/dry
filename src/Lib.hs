module Lib
    ( analyseJSONAst
    )
where

import           FileProcessor                  ( readJSONFile )
import           ASTProcessor                   ( parseRawJSONFile )

analyseJSONAst :: FilePath -> IO ()
analyseJSONAst path = do
    rawJSONFile <- readJSONFile path
    let parsedJSONFile = parseRawJSONFile rawJSONFile
    case parsedJSONFile of
        (Left  errorMessage) -> putStrLn errorMessage
        (Right functionData) -> print functionData

{-
    TODO:
    - traverse all nodes recursively
    - fix statements counter to be recursive
    - reimplement analysis (see es5 branch)
    - reimplement writing output to csv (see es5 branch)
    - consider incorporating the order of appearance into the metric
    - devise a metric to compare function vectors (consider cosine similarity, or k-NN)

-}
