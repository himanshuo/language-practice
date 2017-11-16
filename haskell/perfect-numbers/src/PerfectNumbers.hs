module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify num
    | num <= 0 = Nothing
    | otherwise = 
        let factors = filter (\x -> (num `rem` x) == 0 ) [1..num-1] in 
        let sum = foldl (\acc y -> acc+y) 0 factors in 
            Just (classifyInternal sum num)

classifyInternal :: Int -> Int -> Classification
classifyInternal sum num 
    | sum == num = Perfect
    | sum < num  = Deficient
    | sum > num  = Abundant
