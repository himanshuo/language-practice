module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference n = abs (squareOfSums n - sumOfSquares n)

squareOfSums :: Integral a => a -> a
squareOfSums n = 
    let sum = foldl (\acc x -> x + acc) 0 [1..n] in 
        sum * sum 

sumOfSquares :: Integral a => a -> a
sumOfSquares n = foldl (\acc x -> x*x + acc) 0 [1..n]
