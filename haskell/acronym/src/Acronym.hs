module Acronym (abbreviate) where
import Data.Char 



abbreviate :: String -> String
abbreviate xs =  concat $ map (\word -> getAbbreviateChars word) $ getDistinctWords xs

getAbbreviateChars :: String -> [Char]
getAbbreviateChars word = 
    let firstChar = toUpper $ head word in 
    let capitalLettersInRest =  filter (\ch -> isUpper ch) $ drop 1 word in 
        if (length capitalLettersInRest + 1) == (length word)
        then [firstChar]
        else [firstChar] ++ capitalLettersInRest


getDistinctWords :: String -> [String]
getDistinctWords str = foldl (\acc b -> if b == ' ' || b == '-' then acc ++ [[]] else addCharToLastString b acc ) [""] str

addCharToLastString :: Char -> [String] -> [String]
addCharToLastString b acc =
    if isLetter b 
    then 
        let lastString = last acc in 
        let updatedLastString = lastString ++ [b] in 
        let listBeforeLastString = take (length acc - 1) acc in 
            listBeforeLastString ++ [updatedLastString]
    else acc