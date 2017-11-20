module School (School, add, empty, grade, sorted) where

import Data.Map

type School = Map Int [String]


add :: Int -> String -> School -> School
add gradeNum student school = 
    let current = Data.Map.lookup gradeNum school in 
        case current of 
            Nothing -> insert gradeNum [student] school
            Just children -> insert gradeNum (addToSortedLocation student children) school  

addToSortedLocation :: String -> [String] -> [String]
addToSortedLocation str [] = [str]
addToSortedLocation str (x:xs) = if x < str 
                                 then [x] ++ (addToSortedLocation str xs) 
                                 else [str, x] ++ xs 


-- empty :: School
-- empty school -> Data.Map.empty

grade :: Int -> School -> [String]
grade gradeNum school = 
    case Data.Map.lookup gradeNum school of 
        Nothing -> []
        Just childrenInGrade -> childrenInGrade

sorted :: School -> [(Int, [String])]
sorted school = toAscList school 
