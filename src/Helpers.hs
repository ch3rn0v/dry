{-# LANGUAGE TupleSections #-}

{-|
Module      : Helpers
Description : Provides a number of helper functions
-}
module Helpers where

import           Prelude                 hiding ( lookup )
import           Data.List                      ( sum )
import           Numeric.Extra                  ( intToDouble )

-- | Calculates and returns cartesian product
-- | for all the given list's elements,
-- | with no pairs of same elements.
cartesianProductUnique :: [a] -> [(a, a)]
cartesianProductUnique []       = []
cartesianProductUnique (x : xs) = map (x, ) xs ++ cartesianProductUnique xs

-- | Calculates average value of the list of Double values.
avgDoubles :: [Double] -> Double
avgDoubles [] = error "List must not be null"
avgDoubles ls = sum ls / intToDouble (length ls)

-- | Takes the absolute value of an Int, converts it to Double.
intToAbsDouble :: (Real a, Fractional b) => a -> b
intToAbsDouble = realToFrac . abs

-- | Returns 1 if both args are zero, returns 0 if only one arg is zero,
-- | otherwise divides the lesser int over the greater one.
-- | Args must be non-negative.
divLesserOverGreater :: Real a => a -> a -> Double
divLesserOverGreater a b | a == 0 && b == 0 = 1
                         | a < 0 || b < 0 = undefined
                         | a < b = intToAbsDouble a / intToAbsDouble b
                         | otherwise = intToAbsDouble b / intToAbsDouble a

-- | Performs Min-Max scaling.
minMaxScaling :: Fractional a => a -> a -> a -> a
minMaxScaling min max value = (value - min) / (max - min)
