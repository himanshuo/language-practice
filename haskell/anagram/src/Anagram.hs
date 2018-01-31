module Anagram (anagramsFor) where

import Data.MultiSet (MultiSet, empty, insert)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss =
  foldl (\acc x -> if isAnagram xs x then acc ++ [x] else acc) [] xss

isAnagram :: String -> String -> Bool
isAnagram original potential =
  if obviouslyNotAnagrams original potential
  then False
  else
    let originalMultiSet = generateMultiSet original
        potentialMultiSet = generateMultiSet potential
    in
      originalMultiSet == potentialMultiSet

generateMultiSet :: String -> MultiSet Char
generateMultiSet word = foldl (\acc ch -> insert (toLower ch) acc) empty word


obviouslyNotAnagrams :: String -> String -> Bool
obviouslyNotAnagrams a b =
  if length a /= length b
  then True
  else
      let toLowerWord word = foldl (\acc ch -> acc ++ [toLower ch]) "" word
          aLower = toLowerWord a
          bLower = toLowerWord b
      in
        aLower == bLower
