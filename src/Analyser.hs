module Analyser
    ( analyseParsedSourceFiles
    )
where

import           JSASTProcessor                 ( FunctionData )
import           Helpers                        ( cartesiadProductUnique )

data FunctionPairSimilarity = FunctionPairSimilarity FunctionData FunctionData Double

instance Show FunctionPairSimilarity where
    show (FunctionPairSimilarity f1 f2 sim) = show f1 ++ " ; " ++ show f2 ++ " : " ++ show sim ++ "\n"

estimateFunctionSimilarity
    :: (FunctionData, FunctionData) -> FunctionPairSimilarity
estimateFunctionSimilarity (f1, f2) = FunctionPairSimilarity f1 f2 0

analyseParsedSourceFiles :: [FunctionData] -> [FunctionPairSimilarity]
analyseParsedSourceFiles fs =
    map estimateFunctionSimilarity $ cartesiadProductUnique fs
