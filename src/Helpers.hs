module Helpers where

-- | Consequently applies each function to the result of the previous application,
-- | starting with `f xs`, where `f` is the head of the list of functions and xs is
-- | the initial value of the argument.
mapListOfFunctions :: [[a] -> [a]] -> [a] -> [a]
mapListOfFunctions []       = id
mapListOfFunctions (f : fs) = mapListOfFunctions fs . f

-- | Calculates and returns cartesian product
-- | for all the given list's elements,
-- | with no pairs of same elements.
cartesiadProductUnique :: Eq a => [a] -> [(a, a)]
cartesiadProductUnique xs = [ (a1, a2) | a1 <- xs, a2 <- xs, a1 /= a2 ]
