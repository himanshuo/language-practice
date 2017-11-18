module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs == length ys = 
        Just 
            (foldl (\acc i -> 
                if xs !! i == ys !! i 
                then acc 
                else acc + 1) 0 [0..(length xs - 1)])
    | otherwise = Nothing