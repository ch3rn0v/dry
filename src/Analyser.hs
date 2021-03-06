{-|
Module      : Analyser
Description : Calculates similarity scores for FunctionData pairs
-}
module Analyser
    ( CSV
    , analyseParsedFunctionData
    , functionPairSimilarityDataToCsv
    )
where

import           Data.Ord                       ( Down(Down) )
import           Data.List                      ( sortOn )
import qualified Data.IntMap.Strict            as IM
                                                ( intersectionWithKey
                                                , elems
                                                , keys
                                                )
import           Data.HashMap.Strict            ( elems
                                                , intersectionWith
                                                , difference
                                                )
import           Numeric.Extra                  ( intToDouble )
import           Text.EditDistance              ( defaultEditCosts
                                                , levenshteinDistance
                                                )
import           ASTProcessor                   ( FunctionData
                                                , StatementsCountMap
                                                , filePath
                                                , lineNumber
                                                , name
                                                , arity
                                                , stmtsData
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
                                                           , stmtCountsDiff :: Double
                                                           , stmtsTotalCountDiff :: Double }

data FunctionPairCompoundSimilarity = FunctionPairCompoundSimilarity FunctionData FunctionData Double

instance Eq FunctionPairCompoundSimilarity where
    (==) (FunctionPairCompoundSimilarity _ _ s1) (FunctionPairCompoundSimilarity _ _ s2)
        = s1 == s2

instance Ord FunctionPairCompoundSimilarity where
    (<=) (FunctionPairCompoundSimilarity _ _ s1) (FunctionPairCompoundSimilarity _ _ s2)
        = s1 <= s2

-- | Calculates Levenshtein's distance between the functions' identifiers.
fnsLevenshteinDistance :: FunctionData -> FunctionData -> Double
fnsLevenshteinDistance f1 f2 = 1 / (1 + intToDouble ld)
    where ld = levenshteinDistance defaultEditCosts (name f1) (name f2)

-- | Returns the division of the lesser arity over the greater one,
-- | with one exception: returns 1 if both are zero.
fnsArityDiff :: FunctionData -> FunctionData -> Double
fnsArityDiff f1 f2 = divLesserOverGreater (arity f1) (arity f2)

-- | Returns the division of the lesser count of statements over the greater one,
-- | with one exception: returns 1 if both are zero.
fnsStmtsCountDiff :: FunctionData -> FunctionData -> Double
fnsStmtsCountDiff f1 f2 = divLesserOverGreater (stmtsCount f1) (stmtsCount f2)

-- | Calculates functions' similarity score, based on the number of occurrences
-- | of _shared_ statements in the functions' tree.
calculateSharedStmtsSim :: StatementsCountMap -> StatementsCountMap -> Double
calculateSharedStmtsSim m1 m2 = if null sharedStatementsCount
    then 0
    else avgDoubles sharedStatementsCount
  where
    hashMapWithSharedKeys = intersectionWith divLesserOverGreater m1 m2
    sharedStatementsCount = elems hashMapWithSharedKeys

-- | Calculates functions' similarity score, based on the number of occurrences
-- | of _mutually unique_ statements in the functions' tree.
calculateUniqueStmtsSim :: StatementsCountMap -> StatementsCountMap -> Double
calculateUniqueStmtsSim m1 m2 =
    1 -- because all the metrics are bound to the range of [0; 1]
        - sumAllUniqueElems m1 m2
        / sumAllElems [m1, m2]
  where
    sumElems       = sum . elems
    sumUniqueElems = sumElems . uncurry difference
    sumAllUniqueElems a b = sumUniqueElems (a, b) + sumUniqueElems (b, a)
    sumAllElems = sum . map sumElems

-- | Calculates functions' similarity score, based on the number of occurrences
-- | of statements in the functions' tree (at the same depth level).
fnsStmtsCountsDiff :: StatementsCountMap -> StatementsCountMap -> Double
fnsStmtsCountsDiff m1 m2 =
    (calculateSharedStmtsSim m1 m2 + calculateUniqueStmtsSim m1 m2) / 2

oneOverDepthFractionsSum :: [Int] -> Double
oneOverDepthFractionsSum = sum . map ((1 /) . intToDouble)

fnsStmtsDataDiff :: FunctionData -> FunctionData -> Double
fnsStmtsDataDiff f1 f2 = if maxPossibleDiffSum == 0
    then 1 -- If both functions are empty-bodied, they are similar
    else countsDiffSum / maxPossibleDiffSum
  where
    sd1                = stmtsData f1
    sd2                = stmtsData f2
    stmtsCountsDiffMap = IM.intersectionWithKey
        (\depth scm1 scm2 -> fnsStmtsCountsDiff scm1 scm2 / intToDouble depth)
        sd1
        sd2
    deepestKeys = if length (IM.keys sd1) > length (IM.keys sd2)
        then IM.keys sd1
        else IM.keys sd2
    countsDiffSum      = sum (IM.elems stmtsCountsDiffMap)
    maxPossibleDiffSum = oneOverDepthFractionsSum deepestKeys

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
    (fnsStmtsDataDiff f1 f2)
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
    , 1.0  -- stmtCountsDiff weight
    , 1.0  -- stmtsTotalCountDiff weight
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
        , stmtCountsDiff fprs
        , stmtsTotalCountDiff fprs
        ]
    )

-- | Performs function data analysis to calculate functions' compound diff values.
-- | Returns the list of function pairs with their compound similarity scores, sorted descending.
analyseParsedFunctionData :: [FunctionData] -> [FunctionPairCompoundSimilarity]
analyseParsedFunctionData =
    sortOn Down
        . map
              ( calculateFunctionPairCompoundSimilarity
              . estimateRawFunctionSimilarity
              )
        . cartesianProductUnique

-- | Converts a list of FunctionPairCompoundSimilarity into a single String, ready
-- | to be written as a csv file  (`,` as a column delimiter, `\n` as a row delimiter).
functionPairSimilarityDataToCsv :: [FunctionPairCompoundSimilarity] -> CSV
functionPairSimilarityDataToCsv =
    unlines
        . (:)
              "Fn1 Identifier,Fn1 Statements Count,Fn1 File Path,\
              \Fn2 Identifier,Fn2 Statements Count,Fn2 File Path,\
              \Similarity Score"
        . map
              (\(FunctionPairCompoundSimilarity f1 f2 sim) ->
                  name f1
                      ++ ","
                      ++ show (stmtsCount f1)
                      ++ ","
                      ++ filePath f1
                      ++ ":"
                      ++ show (lineNumber f1)
                      ++ ","
                      ++ name f2
                      ++ ","
                      ++ show (stmtsCount f2)
                      ++ ","
                      ++ filePath f2
                      ++ ":"
                      ++ show (lineNumber f2)
                      ++ ","
                      ++ show sim
              )
