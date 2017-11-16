module Isogram (isIsogram) where
import Data.Char

isIsogram :: String -> Bool
isIsogram str = isIsogramInternal str []

isIsogramInternal :: String -> [Char] -> Bool 
isIsogramInternal "" _ = True
isIsogramInternal ('-':rest) set = isIsogramInternal rest set
isIsogramInternal (' ':rest) set = isIsogramInternal rest set  
isIsogramInternal (s:rest) set = 
    let curChar = toLower s in 
        if curChar `elem` set  
        then False 
        else isIsogramInternal rest (curChar:set)

-- isIsogram str = foldl (\acc x -> if x `elem` acc then Fa) [] str 
