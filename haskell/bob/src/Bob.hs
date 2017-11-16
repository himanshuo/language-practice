module Bob (responseFor) where
import Data.Char

responseFor :: String -> String
responseFor msg 
    | (all_chars_are_caps msg) && (atleast_one_letter msg) = "Whoa, chill out!"
    | (length msg) == 0 || only_spaces msg = "Fine. Be that way!"    
    | (last (strip msg)) == '?' = "Sure."
    | otherwise = "Whatever."


all_chars_are_caps :: String -> Bool 
all_chars_are_caps ([]) = True
all_chars_are_caps (s:[]) = if isLetter s then isUpper s else True 
all_chars_are_caps (s:xs) = if isLetter s then isUpper s && all_chars_are_caps xs else all_chars_are_caps xs

atleast_one_letter :: String -> Bool 
atleast_one_letter ([]) = False 
atleast_one_letter (s:xs) = isLetter s ||  atleast_one_letter xs
atleast_one_letter ([]) = False 

only_spaces :: String -> Bool 
only_spaces [] = True 
only_spaces (x:xs) = (isSpace x) && (only_spaces xs)

strip :: String -> String 
strip x = reverse (fstrip (reverse (fstrip x)))

fstrip :: String -> String 
fstrip ([]) = ""
fstrip (x:xs) = if isSpace x then fstrip xs else x:xs

