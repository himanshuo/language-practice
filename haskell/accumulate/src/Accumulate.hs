module Accumulate (accumulate) where

-- input = 1, 2, 3, 4, 5
-- accumulate sqrFunc input  
-- 1, 4, 9, 16, 25

accumulate :: (a -> b) -> [a] -> [b]
accumulate _ [] = []
accumulate f (x:xs) = [f x] ++ (accumulate f xs)
