module SumOfMultiples (sumOfMultiples) where

    -- sumOfMultiples = foldl (\cur -> if cur is multiple of any factor then add to sum else add 0) [max_factor .. limit]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] limit = 0
sumOfMultiples factors limit = foldl (\acc cur -> if isMultipleOfAny cur factors then acc + cur else acc ) 0 [(minimum factors)..(limit-1)]

isMultipleOfAny :: Integer -> [Integer] -> Bool
isMultipleOfAny cur [] = False 
isMultipleOfAny cur (x:xs) = if isMultiple cur x then True else isMultipleOfAny cur xs

isMultiple :: Integer -> Integer -> Bool
isMultiple cur x = (rem cur x) == 0










