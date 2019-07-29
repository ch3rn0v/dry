module Helpers where

import           Data.List                      ( sum )
import           Numeric.Extra                  ( intToDouble )

-- | Consequently applies each function to the result of the previous application,
-- | starting with `f xs`, where `f` is the head of the list of functions and xs is
-- | the initial value of the argument.
mapListOfFunctions :: [[a] -> [a]] -> [a] -> [a]
mapListOfFunctions []       = id
mapListOfFunctions (f : fs) = mapListOfFunctions fs . f

-- | Calculates and returns cartesian product
-- | for all the given list's elements,
-- | with no pairs of same elements.
cartesianProductUnique :: [a] -> [(a, a)]
cartesianProductUnique [] = []
cartesianProductUnique (x:xs) = map ((,) x) xs ++ cartesianProductUnique xs

-- | Calculates average value of the list of Double values.
avgDoubles :: [Double] -> Double
avgDoubles ls = sum ls / intToDouble (length ls)

-- | Performs Min-Max scaling.
minMaxScaling :: Fractional a => a -> a -> a -> a
minMaxScaling min max value = (value - min) / (max - min)
