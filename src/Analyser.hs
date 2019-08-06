module Analyser
    ( CSV
    , analyseParsedSourceFiles
    , functionPairSimilarityDataToCsv
    )
where

import           Data.Ord                       ( Down(Down) )
import           Data.List                      ( sortOn )
import           Data.Data                      ( Constr )
import           Data.Map.Strict                ( Map
                                                , elems
                                                , intersectionWith
                                                , difference
                                                )
import           Numeric.Extra                  ( intToDouble )
import           Text.EditDistance              ( defaultEditCosts
                                                , levenshteinDistance
                                                )
import           JSASTProcessor                 ( FunctionData
                                                , ConstrCountMap
                                                , filePath
                                                , lineNumber
                                                , fName
                                                , arity
                                                , entitiesCountMap
                                                , stmtsCount
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
                                                           , arityDiff :: Double
                                                           , constrCountsDiff :: Double
                                                           , stmtsCountDiff :: Double }

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

-- | Returns the division of the lesser arity over the greater one,
-- | except two cases:
-- | - returns 1 if both are zero,
-- | - returns 0 if only one is zero.
fnsArityDiff :: FunctionData -> FunctionData -> Double
fnsArityDiff f1 f2 = divLesserOverGreater (arity f1) (arity f2)

-- | Returns the division of the lesser count of statements over the greater one,
-- | except two cases:
-- | - returns 1 if both are zero,
-- | - returns 0 if only one is zero.
fnsStmtsCountDiff :: FunctionData -> FunctionData -> Double
fnsStmtsCountDiff f1 f2 = divLesserOverGreater (stmtsCount f1) (stmtsCount f2)

-- | Calculates functions' similarity score, based on the number of occurrences
-- | of _shared_ constructors in the function's `JSStatement` tree.
calculateSharedConstrSim :: ConstrCountMap -> ConstrCountMap -> Double
calculateSharedConstrSim m1 m2 = if null sharedConstructorsCount
    then 0
    else avgDoubles sharedConstructorsCount
  where
    mapWithSharedKeys       = intersectionWith divLesserOverGreater m1 m2
    sharedConstructorsCount = elems mapWithSharedKeys

-- | Calculates functions' similarity score, based on the number of occurrences
-- | of _mutually unique_ constructors in the function's `JSStatement` tree.
calculateUniqueConstrSim :: ConstrCountMap -> ConstrCountMap -> Double
calculateUniqueConstrSim m1 m2 =
    1 -- because all the metrics are bound to the range of [0; 1]
        - sumAllUniqueElems m1 m2
        / sumAllElems [m1, m2]
  where
    sumElemsToDouble = sum . elems
    sumUniqueElems   = sumElemsToDouble . uncurry difference
    sumAllUniqueElems a b = sumUniqueElems (a, b) + sumUniqueElems (b, a)
    sumAllElems = sum . map sumElemsToDouble

-- | Calculates functions' similarity score, based on the number of occurrences
-- | of constructors in the function's `JSStatement` tree.
fnsConstrCountsDiff :: FunctionData -> FunctionData -> Double
fnsConstrCountsDiff f1 f2 =
    (calculateSharedConstrSim m1 m2 + calculateUniqueConstrSim m1 m2) / 2
  where
    m1 = entitiesCountMap f1
    m2 = entitiesCountMap f2

-- | Calculates similarity scores along every axis for every pair of functions
-- | that are compared. Returns FunctionPairRawSimilarity that stores the diffs
-- | in the raw form.
estimateRawFunctionSimilarity
    :: (FunctionData, FunctionData) -> FunctionPairRawSimilarity
estimateRawFunctionSimilarity (f1, f2) = FunctionPairRawSimilarity
    f1
    f2
    (fnsLevenshteinDistance f1 f2)
    (fnsArityDiff f1 f2)
    (fnsConstrCountsDiff f1 f2)
    (fnsStmtsCountDiff f1 f2)

-- | Calculates compound diff value given the raw diff values.
aggregateNormalizedDiffs :: [Double] -> [Double] -> Double
aggregateNormalizedDiffs diffWeightsVector diffValues = minMaxScaling
    0
    (avgDoubles diffWeightsVector)
    aggNormalizedDiff  where
    aggNormalizedDiff = avgDoubles $ zipWith (*) diffWeightsVector diffValues

-- | Determines how important each metric is relative to other metrics.
diffWeightsVector :: [Double]
diffWeightsVector =
    [ 0.15 -- nameDiff weight
    , 0.3  -- arityDiff weight
    , 1.0  -- constrCountsDiff weight
    , 1.0  -- stmtsCountDiff weight
    ]

-- | Aggregates raw similarity values of a functions pair into single compound similarity value.
calculateFunctionPairCompoundSimilarity
    :: FunctionPairRawSimilarity -> FunctionPairCompoundSimilarity
calculateFunctionPairCompoundSimilarity fprs = FunctionPairCompoundSimilarity
    (f1 fprs)
    (f2 fprs)
    (aggregateNormalizedDiffs
        diffWeightsVector
        [ nameDiff fprs
        , arityDiff fprs
        , constrCountsDiff fprs
        , stmtsCountDiff fprs
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
                      ++ ":"
                      ++ lineNumber f1
                      ++ ","
                      ++ fName f2
                      ++ ","
                      ++ filePath f2
                      ++ ":"
                      ++ lineNumber f2
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
