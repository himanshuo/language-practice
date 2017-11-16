module RunLength (decode, encode) where
import Data.Char

-- ""
-- w
-- 2w
-- 10w
-- 21wz
-- 2w2q
-- 12w12q
-- w12222x

decode :: String -> String
decode "" = ""
decode encodedText = 
    let count = getNextCount encodedText in 
    let character = getNextChar encodedText in 
    let remaining = getRemaining encodedText count character in 
        (dup character count) ++ (decode remaining)


getNextCount :: String -> Int
getNextCount "" = 0
getNextCount str = 
    let nextDigits = getNextDigits str 
    in 
        if (length nextDigits) == 0 
        then 1
        else strToInt nextDigits

strToInt :: String -> Int 
strToInt str = read str :: Int


getNextDigits :: String -> String 
getNextDigits ([]) = []
getNextDigits (x:xs) = if isDigit x 
                        then x:(getNextDigits xs)
                        else []

-- Given RunLength Encoded String, get the first non-numeric character
getNextChar :: String -> Char 
getNextChar ([]) = '~'
getNextChar (x:xs) = if (isLetter x || isSpace x) then x else getNextChar xs


getRemaining :: String -> Int -> Char -> String 
getRemaining str count chr = if count == 1 
                                then drop 1 str
                                else drop ( (length (show count)) + 1 ) str 

dup :: Char -> Int -> String 
dup chr 0 = ""
dup chr count = chr:(dup chr (count-1))

-- -- _    -> 0
-- -- 0    -> 0
-- -- 1    -> 1
-- -- 01   -> 10
-- -- 11   -> 11
-- -- 12   -> 21
-- -- 123   -> 321
-- strToIntInternal :: String -> Int 
-- strToIntInternal ([]) = 0
-- strToIntInternal (x:[]) = digitToInt x
-- strToIntInternal (x:xs) = (digitToInt x) + (10 * (strToIntInternal xs))

    -- if isDigit x 
    --                 then 
    --                     getNextCount xs
    --                     digitToInt x
                        
    --                 else 1


encode :: String -> String
encode ([]) = ""
encode str = 
    let character = head str in         
    let count = numStartingInstancesOf character str  in 
    let remaining = drop count str in
            if count == 1
            then    [character] ++ (encode remaining)
            else    ( (show count) ++ [character]) ++ (encode remaining)
            
      


numStartingInstancesOf :: Char -> String -> Int
numStartingInstancesOf chr ([]) = 0
numStartingInstancesOf chr (x:xs) = if chr == x
                                    then 1 + (numStartingInstancesOf chr xs)
                                    else 0