module Grains (square, total) where

square :: Integer -> Maybe Integer
square x
    | x <= 0 = Nothing
    | x > 64 = Nothing
    | otherwise = Just $ 2 ^ (x-1)


squareWrapper :: Integer -> Integer 
squareWrapper x = 
    let val = square x in 
        case val of 
            Nothing -> 0 
            Just i -> i
-- 1: 1
-- 2: 2
-- 3: 4
-- 4: 8
-- 5: 16 
-- 6: 32

-- total :: Integer
-- total = foldl (\acc x -> acc + (squareWrapper x)) 0 [1..64]


-- http://mathworld.wolfram.com/ExponentialSumFormulas.html
-- 2^0 + 2^1 + 2^3 + ... 2^64 = SUM(n = 0->64, r^n)
-- rule: sum(n=0->N-1, r^n) = (1-r^N) / (1-r)

total :: Integer
total = let r = 2 in 
        let capN = 64 in 
        (1 - (r ^ capN) ) `div` (1-r)
