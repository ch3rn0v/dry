module Analyser
    ( analyseParsedSourceFiles
    )
where

import           Numeric.Extra                  ( intToDouble )
import           Text.EditDistance              ( defaultEditCosts
                                                , levenshteinDistance
                                                )
import           JSASTProcessor                 ( FunctionData
                                                , fName
                                                , arity
                                                , purity
                                                , explicitReturn
                                                , stmts
                                                )
import           Helpers                        ( cartesiadProductUnique )

data FunctionPairSimilarity = FunctionPairSimilarity FunctionData FunctionData Double

instance Show FunctionPairSimilarity where
    show (FunctionPairSimilarity f1 f2 sim) =
        show f1 ++ " ; " ++ show f2 ++ " : " ++ show sim ++ "\n"

fnsLevenshteinDistance :: FunctionData -> FunctionData -> Double
fnsLevenshteinDistance f1 f2 =
    intToDouble $ levenshteinDistance defaultEditCosts (fName f1) (fName f2)

fnsBoolPropDiff
    :: Eq a => (FunctionData -> a) -> FunctionData -> FunctionData -> Double
fnsBoolPropDiff boolProp f1 f2 = if boolProp f1 == boolProp f2 then 0 else 1

fnsIntPropDiff
    :: (FunctionData -> Int) -> FunctionData -> FunctionData -> Double
fnsIntPropDiff intProp f1 f2 = intToDouble $ abs $ intProp f1 - intProp f2

fnsPurityDiff :: FunctionData -> FunctionData -> Double
fnsPurityDiff = fnsBoolPropDiff purity

fnsReturnDiff :: FunctionData -> FunctionData -> Double
fnsReturnDiff = fnsBoolPropDiff explicitReturn

fnsArityDiff :: FunctionData -> FunctionData -> Double
fnsArityDiff = fnsIntPropDiff arity

fnsStmtsCountDiff :: FunctionData -> FunctionData -> Double
fnsStmtsCountDiff = fnsIntPropDiff (length . stmts)

estimateFunctionSimilarity
    :: (FunctionData, FunctionData) -> FunctionPairSimilarity
estimateFunctionSimilarity (f1, f2) =
    let nameDiffW     = 1.0
        purityDiffW   = 1.0
        returnDiffW   = 1.0
        arityDiffW    = 1.0
        stmtsLenDiffW = 1.0
        fnCompoundSim =
                nameDiffW
                    * fnsLevenshteinDistance f1 f2
                    + purityDiffW
                    * fnsPurityDiff f1 f2
                    + returnDiffW
                    * fnsReturnDiff f1 f2
                    + arityDiffW
                    * fnsArityDiff f1 f2
                    + stmtsLenDiffW
                    * fnsStmtsCountDiff f1 f2
    in  FunctionPairSimilarity f1 f2 fnCompoundSim

analyseParsedSourceFiles :: [FunctionData] -> [FunctionPairSimilarity]
analyseParsedSourceFiles fs =
    map estimateFunctionSimilarity $ cartesiadProductUnique fs
