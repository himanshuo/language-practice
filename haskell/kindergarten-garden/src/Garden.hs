module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Char (toUpper)
import Data.Map (Map, empty, insert)
data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)


-- "Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"

-- RC\nGG : Alice -> [R, C, G, G]
-- VVCG\nVVRC : Alice -> [V, V, V, V], Bob -> [C G R C]



defaultGarden :: String -> Map String [Plant]
defaultGarden plants = 
    let map = Data.Map.empty in 
    let students = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"] in 
    let plants' = lines plants in 
    foldl (updateMap plants' students) map [0..(length students)]

    


updateMap :: [String] -> [String] -> Map String [Plant] -> Int -> Map String [Plant]
updateMap plants students map studentIndex = 
    let studentsPlantsAsStrings = getStudentsPlantsAtIndex (studentIndex*2) ++  (getStudentsPlantsAtIndex (studentIndex * 2 + 1)) in
    let studentsPlants = foldl addPlants [] studentsPlantsAsStrings in
    insert (students !! studentIndex) studentsPlants map

getStudentsPlantsAtIndex :: Int -> [String] -> [Plant]
getStudentsPlantsAtIndex index rowsOfPlants =  foldl (\myplants curRow -> myplants ++ [curRow !! index] ) [] rowsOfPlants in 
        
addPlants :: [Plant] -> Char -> [Plant] 
addPlants listOfPlants plantChr = 
    let upperPlantChr = toUpper plantChr in 
    case chrToPlant upperPlantChr of 
        Left _ -> listOfPlants         
        Right plant -> listOfPlants ++ [plant]


chrToPlant :: Char -> Either String Plant 
chrToPlant 'C' = Right Clover
chrToPlant 'G' = Right Grass
chrToPlant 'R' = Right Radishes
chrToPlant 'V' = Right Violets
chrToPlant chr = Left $ "Invalid String in list of plants: " ++ [chr]

garden :: [String] -> String -> Map String [Plant]
garden students plants = error "You need to implement this function."

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student garden = error "You need to implement this function."
