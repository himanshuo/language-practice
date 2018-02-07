module Allergies (Allergen(..), allergies, isAllergicTo) where

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show)

allergies :: Int -> [Allergen]
allergies score = allergiesInternal score []

allergiesInternal :: Int -> [Allergen] -> [Allergen]
allergiesInternal 0 collectedAlergens = collectedAlergens
allergiesInternal score collectedAlergens =
  let binary = getMaxBinaryLessThanOrEqualTo score 1 0
      result = allergenFromScore binary
      nextScore = score - binary
  in
    case result of
      Left _ -> allergiesInternal nextScore collectedAlergens
      Right allergen -> allergiesInternal nextScore (collectedAlergens ++ [allergen])

getMaxBinaryLessThanOrEqualTo :: Int -> Int -> Int -> Int
getMaxBinaryLessThanOrEqualTo x currentResult currentFactor =
   let nextFactor = currentFactor + 1
       nextResult = 2 ^ nextFactor
   in
      if nextResult > x
      then currentResult
      else getMaxBinaryLessThanOrEqualTo x nextResult nextFactor


isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = allergen `elem` (allergies score)

allergenFromScore :: Int -> Either String Allergen
allergenFromScore 1 = Right Eggs
allergenFromScore 2 = Right Peanuts
allergenFromScore 4 = Right Shellfish
allergenFromScore 8 = Right Strawberries
allergenFromScore 16 = Right Tomatoes
allergenFromScore 32 = Right Chocolate
allergenFromScore 64 = Right Pollen
allergenFromScore 128 = Right Cats
allergenFromScore _ = Left "Invalid score"
