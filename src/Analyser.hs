module Analyser
    ( FunctionPairCompoundSimilarity
    , CSV
    , analyseParsedSourceFiles
    , functionPairSimilarityDataToCsv
    )
where

import           Data.Ord                       ( Down(Down) )
import           Data.List                      ( foldl'
                                                , sortOn
                                                )
import           Numeric.Extra                  ( intToDouble )
import           Text.EditDistance              ( defaultEditCosts
                                                , levenshteinDistance
                                                )
import           JSASTProcessor                 ( FunctionData
                                                , filePath
                                                , fName
                                                , arity
                                                , functionCallsCount
                                                , explicitReturn
                                                , stmts
                                                , declarationsCount
                                                )
import           Helpers                        ( avgDoubles
                                                , cartesianProductUnique
                                                , divLesserOverGreater
                                                , minMaxScaling
                                                )

type CSV = String

data FunctionPairRawSimilarity = FunctionPairRawSimilarity { f1 :: FunctionData
                                                           , f2 :: FunctionData
                                                           , nameDiff :: Double
                                                           , functionCallsDiff :: Double
                                                           , returnDiff :: Double
                                                           , arityDiff :: Double
                                                           , stmtsLenDiff :: Double
                                                           , declarationsCountDiff :: Double }

-- | Determines how important each metric is relative to other metrics.
diffWeightsVector :: [Double]
diffWeightsVector =
    [ 0.05 -- nameDiff weight
    , 0.2  -- functionCallsDiff weight
    , 0.2  -- returnDiff weight
    , 0.1  -- arityDiff weight
    , 0.95 -- stmtsLenDiff weight
    , 1.0  -- declarationsCountDiff weight
    ]

data FunctionPairCompoundSimilarity = FunctionPairCompoundSimilarity FunctionData FunctionData Double

instance Eq FunctionPairCompoundSimilarity where
    (==) (FunctionPairCompoundSimilarity _ _ s1) (FunctionPairCompoundSimilarity _ _ s2)
        = s1 == s2

instance Ord FunctionPairCompoundSimilarity where
    (<=) (FunctionPairCompoundSimilarity _ _ s1) (FunctionPairCompoundSimilarity _ _ s2)
        = s1 <= s2

-- | Calculates Levenshein's distance between the functions' identifiers.
fnsLevenshteinDistance :: FunctionData -> FunctionData -> Double
fnsLevenshteinDistance f1 f2 = if ld == 0 then 1 else 1 / intToDouble ld
    where ld = levenshteinDistance defaultEditCosts (fName f1) (fName f2)

-- | Returns zero if the boolean type property values of both functions are of the same value.
-- | Returns one otherwise.
fnsBoolPropDiff
    :: Eq a => (FunctionData -> a) -> FunctionData -> FunctionData -> Double
fnsBoolPropDiff boolProp f1 f2 = if boolProp f1 == boolProp f2 then 1 else 0

-- | Returns the abs difference between integer type property values of the functions.
fnsIntPropDiff
    :: (FunctionData -> Int) -> FunctionData -> FunctionData -> Double
fnsIntPropDiff intProp f1 f2 = divLesserOverGreater (intProp f1) (intProp f2)

-- | See `fnsBoolPropDiff`
fnsFunctionCallsDiff :: FunctionData -> FunctionData -> Double
fnsFunctionCallsDiff = fnsIntPropDiff functionCallsCount

-- | See `fnsBoolPropDiff`
fnsReturnDiff :: FunctionData -> FunctionData -> Double
fnsReturnDiff = fnsBoolPropDiff explicitReturn

-- | See `fnsIntPropDiff`
fnsArityDiff :: FunctionData -> FunctionData -> Double
fnsArityDiff = fnsIntPropDiff arity

-- | See `fnsIntPropDiff`
fnsStmtsCountDiff :: FunctionData -> FunctionData -> Double
fnsStmtsCountDiff = fnsIntPropDiff (length . stmts)

fnsDeclarationsCountDiff :: FunctionData -> FunctionData -> Double
fnsDeclarationsCountDiff = fnsIntPropDiff declarationsCount

-- | Calculates similarity scores along every axis for every pair of functions
-- | that are compared. Returns FunctionPairRawSimilarity that stores the diffs
-- | in the raw form.
estimateRawFunctionSimilarity
    :: (FunctionData, FunctionData) -> FunctionPairRawSimilarity
estimateRawFunctionSimilarity (f1, f2) = FunctionPairRawSimilarity
    f1
    f2
    (fnsLevenshteinDistance f1 f2)
    (fnsFunctionCallsDiff f1 f2)
    (fnsReturnDiff f1 f2)
    (fnsArityDiff f1 f2)
    (fnsStmtsCountDiff f1 f2)
    (fnsDeclarationsCountDiff f1 f2)

-- | Calculates compound diff value given the raw diff values.
aggregateNormalizedDiffs :: [Double] -> [Double] -> Double
aggregateNormalizedDiffs diffWeightsVector diffValues = minMaxScaling
    0
    (avgDoubles diffWeightsVector)
    aggNormalizedDiff  where
    aggNormalizedDiff = avgDoubles $ zipWith (*) diffWeightsVector diffValues

-- | Aggregates raw similarity values of a functions pair into single compound similarity value.
calculateFunctionPairCompoundSimilarity
    :: FunctionPairRawSimilarity -> FunctionPairCompoundSimilarity
calculateFunctionPairCompoundSimilarity fprs = FunctionPairCompoundSimilarity
    (f1 fprs)
    (f2 fprs)
    (aggregateNormalizedDiffs
        diffWeightsVector
        [ nameDiff fprs
        , functionCallsDiff fprs
        , returnDiff fprs
        , arityDiff fprs
        , stmtsLenDiff fprs
        , declarationsCountDiff fprs
        ]
    )

-- | Converts a list of FunctionPairCompoundSimilarity into a single String, ready
-- | to be written as a csv file  (`,` as a column delimiter, `\n` as a row delimiter).
functionPairSimilarityDataToCsv :: [FunctionPairCompoundSimilarity] -> CSV
functionPairSimilarityDataToCsv =
    unlines
        . (:)
              "Fn1 identifier,Fn1 file path,\
              \Fn2 identifier,Fn2 file path,\
              \Similarity score"
        . map
              (\(FunctionPairCompoundSimilarity f1 f2 sim) ->
                  fName f1
                      ++ ","
                      ++ filePath f1
                      ++ ","
                      ++ fName f2
                      ++ ","
                      ++ filePath f2
                      ++ ","
                      ++ show sim
              )

-- | Performs function data analysis to calculate functions' compound diff values.
-- | Returns the list of function pairs with their compound similarity scores, sorted descending.
analyseParsedSourceFiles :: [FunctionData] -> [FunctionPairCompoundSimilarity]
analyseParsedSourceFiles =
    sortOn Down
        . map
              ( calculateFunctionPairCompoundSimilarity
              . estimateRawFunctionSimilarity
              )
        . cartesianProductUnique
