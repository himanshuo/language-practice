module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Char (toUpper)
import Data.Map (Map, empty, insert, lookup)
import Data.List (elemIndex, sort)
data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

chrToPlant :: Char -> Either String Plant
chrToPlant 'C' = Right Clover
chrToPlant 'G' = Right Grass
chrToPlant 'R' = Right Radishes
chrToPlant 'V' = Right Violets
chrToPlant chr = Left $ "Invalid String in list of plants: " ++ [chr]

getPlantsForStudent :: Int -> String -> [Plant]
getPlantsForStudent studentIndex row =
  let getPlantAtIndex index = chrToPlant (row !! index)
      addOnlyPlantToList possiblePlant list = case possiblePlant of
                                                  Left _ -> list
                                                  Right plant -> list ++ [plant]
      firstPlant = getPlantAtIndex (studentIndex * 2)
      secondPlant = getPlantAtIndex (studentIndex * 2 + 1)
  in
      foldl (\acc possiblePlant -> addOnlyPlantToList possiblePlant acc) [] [firstPlant, secondPlant]


defaultGarden :: String -> Map String [Plant]
defaultGarden plants =
  let defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]
      indexOfNewLine = Data.List.elemIndex '\n' plants
      numRequiredStudents = case indexOfNewLine of
                                Nothing -> length plants `quot` 2
                                Just a -> a `quot` 2
      students = take numRequiredStudents defaultStudents
  in
    garden students plants


garden :: [String] -> String -> Map String [Plant]
garden students plants =
  let rows = lines plants
      sortedStudents = sort students
      studentIndexes = [0..(length sortedStudents - 1)]
  in
      foldl (\acc studentIndex ->
              let student = sortedStudents !! studentIndex
                  plantsForStudent = foldl (\ac row -> ac ++ (getPlantsForStudent studentIndex row)) [] rows
              in
                Data.Map.insert student plantsForStudent acc
              ) Data.Map.empty studentIndexes

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student garden = case Data.Map.lookup student garden of
                                  Nothing -> []
                                  Just plants -> plants
