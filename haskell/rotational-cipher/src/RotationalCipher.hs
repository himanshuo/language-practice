module RotationalCipher (rotate) where
import Data.Char 

rotate :: Int -> String -> String
rotate n str = map (\x -> incX x n) str 


incX :: Char -> Int -> Char
incX cur n =
    if isLetter cur 
    then
        let increased_val = (ord cur) + (rem n 26) in 
        if over cur increased_val 
        then chr (increased_val - 26)
        else chr increased_val
    else cur

over :: Char -> Int -> Bool 
over cur increased_val 
    | ('a' <= cur && cur <= 'z' ) = increased_val > (ord 'z')
    | ('A' <= cur && cur <= 'Z' ) = increased_val > (ord 'Z')