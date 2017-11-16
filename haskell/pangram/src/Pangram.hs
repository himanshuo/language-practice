module Pangram (isPangram) where
import Data.Char

isPangram :: String -> Bool
isPangram text = isPangramImp text []

isPangramImp :: String -> [Char] -> Bool
isPangramImp "" map = (length map) == 26
isPangramImp (x:xs) map = 
    if isLetter(x)
    then 
        let new_map = if (toLower x) `elem` map 
                    then map 
                    else (toLower x):map 
        in 
            if (length map) == 26
            then True 
            else isPangramImp xs new_map
    else  isPangramImp xs map

-- isPangramImp x:xs map =
--     let new_map = if x `elem` map
--                   then map 
--                   else x:map
--     in
--     if (length text) == 26 
--     then True 
--     else isPangramImp xs new_map


    


-- isPangramImp text map = 
--         if no elements in text, return map is full 
--         get first element of text
--         if e not in map, add it to new_map
--         if new_map is full return True 
--         else return (call isPangramImp remaining_text new_map)


-- ALPHABET: abc

-- isPangramImp text map

-- isPangramImp abc {}
--     isPangramImp bc {a}
--         isPangramImp c {a,b}
--             TRUE

-- isPangramImp ab {}
--     isPangramImp b {a}
--         isPangramImp "" {a,b}
--             FALSE

-- isPangramImp a {}

-- isPangramImp abcc {}
--     isPangramImp bcc {a}
--         isPangramImp cc {a,b}
--             TRUE
        
