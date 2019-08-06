module Helpers where

import           Prelude                 hiding ( lookup )
import           Data.List                      ( sum )
import           Numeric.Extra                  ( intToDouble )
import           Data.Map.Strict                ( Map
                                                , lookup
                                                , mapWithKey
                                                )

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
cartesianProductUnique []       = []
cartesianProductUnique (x : xs) = map ((,) x) xs ++ cartesianProductUnique xs

-- | Calculates average value of the list of Double values.
avgDoubles :: [Double] -> Double
avgDoubles [] = error "List must not be null"
avgDoubles ls = sum ls / intToDouble (length ls)

-- | Takes the absolute value of an Int, converts it to Double.
intToAbsDouble :: (Real a, Fractional b) => a -> b
intToAbsDouble = realToFrac . abs

-- | Returns 1 if both args are zero, returns 0 if only one arg is zero,
-- | otherwise divides the lesser int over the greater one.
divLesserOverGreater :: Real a => a -> a -> Double
divLesserOverGreater a b | a == 0 && b == 0 = 1
                         | a == 0 || b == 0 = 0
                         | a < b = intToAbsDouble a / intToAbsDouble b
                         | otherwise = intToAbsDouble b / intToAbsDouble a

-- | Performs Min-Max scaling.
minMaxScaling :: Fractional a => a -> a -> a -> a
minMaxScaling min max value = (value - min) / (max - min)

-- | Applies `f` to all the values from the `givenMap` and the values,
-- | accessed by the corresponding keys at the `defaultMap`.
-- | If the key is absent, the `defaultValue` is used.
combineWithKeyAndDefault
  :: Ord k => (a -> b -> c) -> b -> Map k b -> Map k a -> Map k c
combineWithKeyAndDefault f defaultValue defaultMap = mapWithKey
  combineWithValueOrDefault
 where
  combineWithValueOrDefault k v =
    maybe (f v defaultValue) (f v) $ lookup k defaultMap
