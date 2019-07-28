module Analyser
    ( analyseParsedSourceFiles
    )
where

import           Data.Ord                       ( Down(Down) )
import           Data.List                      ( foldl', sortOn )
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
import           Helpers                        ( avgDoubles
                                                , minMaxScaling
                                                , cartesianProductUnique
                                                )

data FunctionPairRawSimilarity = FunctionPairRawSimilarity { f1 :: FunctionData
                                                           , f2 :: FunctionData
                                                           , nameDiff :: Double
                                                           , purityDiff :: Double
                                                           , returnDiff :: Double
                                                           , arityDiff :: Double
                                                           , stmtsLenDiff :: Double }
data FunctionPairCompoundSimilarity = FunctionPairCompoundSimilarity FunctionData FunctionData Double

instance Show FunctionPairCompoundSimilarity where
    show (FunctionPairCompoundSimilarity f1 f2 sim) =
        show f1 ++ " ; " ++ show f2 ++ " : " ++ show sim ++ "\n"

instance Eq FunctionPairCompoundSimilarity where
    (==) (FunctionPairCompoundSimilarity _ _ s1) (FunctionPairCompoundSimilarity _ _ s2) = s1 == s2

instance Ord FunctionPairCompoundSimilarity where
    (<=) (FunctionPairCompoundSimilarity _ _ s1) (FunctionPairCompoundSimilarity _ _ s2) = s1 <= s2

-- | Calculates Levenshein's distance between the functions' identifiers.
fnsLevenshteinDistance :: FunctionData -> FunctionData -> Double
fnsLevenshteinDistance f1 f2 =
    intToDouble $ levenshteinDistance defaultEditCosts (fName f1) (fName f2)

-- | Returns zero if the boolean type property values of both functions are of the same value.
-- | Returns one otherwise.
fnsBoolPropDiff
    :: Eq a => (FunctionData -> a) -> FunctionData -> FunctionData -> Double
fnsBoolPropDiff boolProp f1 f2 = if boolProp f1 == boolProp f2 then 0 else 1

-- | Returns the abs difference between integer type property values of the functions.
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

-- | Calculates similarity scores along every axis for every pair of functions
-- | that are compared. Returns FunctionPairRawSimilarity that stores the diffs
-- | in the raw form.
estimateRawFunctionSimilarity
    :: (FunctionData, FunctionData) -> FunctionPairRawSimilarity
estimateRawFunctionSimilarity (f1, f2) =
    let nameDiffW     = 1.0
        purityDiffW   = 1.0
        returnDiffW   = 1.0
        arityDiffW    = 1.0
        stmtsLenDiffW = 1.0
    in  FunctionPairRawSimilarity f1
                                  f2
                                  (nameDiffW * fnsLevenshteinDistance f1 f2)
                                  (purityDiffW * fnsPurityDiff f1 f2)
                                  (returnDiffW * fnsReturnDiff f1 f2)
                                  (arityDiffW * fnsArityDiff f1 f2)
                                  (stmtsLenDiffW * fnsStmtsCountDiff f1 f2)

-- | Calculates max values along every diff axis.
getMaxRawSimValues
    :: [FunctionPairRawSimilarity] -> (Double, Double, Double, Double, Double)
getMaxRawSimValues = foldl'
    (\(maxND, maxPD, maxRD, maxAD, maxSLD) fprs ->
        let maxND'  = max maxND (nameDiff fprs)
            maxPD'  = max maxPD (purityDiff fprs)
            maxRD'  = max maxRD (returnDiff fprs)
            maxAD'  = max maxAD (arityDiff fprs)
            maxSLD' = max maxSLD (stmtsLenDiff fprs)
        in  (maxND', maxPD', maxRD', maxAD', maxSLD')
    )
    (0.0, 0.0, 0.0, 0.0, 0.0)

-- | Scales values to [0; 1], assuming minimal possible unscaled value to be zero.
-- | Accepts max value as the first argument, value to be scaled as the second.
scaleDiffValue :: Double -> Double -> Double
scaleDiffValue = minMaxScaling 0

-- | Calculates compound diff value given the raw diff values.
aggregateNormalizedDiffs :: [Double] -> Double
aggregateNormalizedDiffs = avgDoubles

-- | Aggregates raw similarity values of a functions pair into single compound similarity value.
calculateFunctionPairCompoundSimilarity
    :: [FunctionPairRawSimilarity] -> [FunctionPairCompoundSimilarity]
calculateFunctionPairCompoundSimilarity fprss =
    let (maxND, maxPD, maxRD, maxAD, maxSLD) = getMaxRawSimValues fprss
    in
        map
            (\fprs ->
                let nDiffs = map (uncurry scaleDiffValue) [ (maxND, nameDiff fprs)
                        , (maxPD, purityDiff fprs)
                        , (maxRD, returnDiff fprs)
                        , (maxAD, arityDiff fprs)
                        , (maxSLD, stmtsLenDiff fprs) ]
                in  FunctionPairCompoundSimilarity
                        (f1 fprs)
                        (f2 fprs)
                        (aggregateNormalizedDiffs nDiffs)
            )
            fprss

-- | Performs function data analysis to calculate functions' compound diff values.
-- | Returns the list of function pairs with their compound similarity scores, sorted descending.
analyseParsedSourceFiles :: [FunctionData] -> [FunctionPairCompoundSimilarity]
analyseParsedSourceFiles = sortOn Down
    . calculateFunctionPairCompoundSimilarity
    . map estimateRawFunctionSimilarity
    . cartesianProductUnique
