module Helpers where

-- | Consequently applies each function to the result of the previous application,
-- | starting with `f xs`, where `f` is the head of the list of functions and xs is
-- | the initial value of the argument.
mapListOfFunctions :: [[a] -> [a]] -> [a] -> [a]
mapListOfFunctions []       = id
mapListOfFunctions (f : fs) = mapListOfFunctions fs . f