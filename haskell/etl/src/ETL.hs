module ETL (transform) where
import Data.Char 
import Data.Map (
    Map, 
    empty, 
    lookup, 
    insert,
    foldlWithKey)

-- 3: A B C
-- 2: D E
-- ... 

-- transforms to 

-- a: 3
-- b: 3
-- d: 2
-- ...

transform :: Map a String -> Map Char a
transform legacyData = foldlWithKey transformInternal empty legacyData

transformInternal :: Map Char a -> a -> String -> Map Char a
transformInternal acc key value = 
    foldl (selectivelyAdd key) acc value

selectivelyAdd :: a -> Map Char a -> Char -> Map Char a
selectivelyAdd number acc letter = 
    let validLetter = toLower letter in 
    case Data.Map.lookup validLetter acc of 
        Nothing -> insert validLetter number acc 
        Just _ -> acc 
    

