module Scrabble (scoreLetter, scoreWord) where
import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter letter
    | any (== l) "AEIOULNRST" = 1
    | any (== l) "DG" = 2
    | any (== l) "BCMP" = 3
    | any (== l) "FHVWY" = 4
    | any (== l) "K" = 5
    | any (== l) "JX" = 8
    | any (== l) "QZ" = 10
    | otherwise = 0
        where l = toUpper letter
    
scoreWord :: String -> Integer
scoreWord word = foldl (\acc l -> acc + scoreLetter l) 0 word
