module Diamond (diamond) where
import Data.Char
-- diamond letter = print_line A for [A...letter...A]
diamond :: Char -> [String]
diamond letter = 
    let beforeLetter = chr (ord letter - 1) in
    let muchBefore = chr (ord letter - 2) in 
    let letters = ['A'..letter] ++ [beforeLetter, muchBefore..'A'] in 
    let max = space_in_between letter + 2 in 
        map (print_line max) letters

-- print_line letter = print letter, print spaces, print letter
print_line :: Int -> Char -> String 
print_line max 'A' = 
    let end_spaces = dup (num_end_spaces max 'A') ' ' in 
        end_spaces ++ ['A'] ++ end_spaces
print_line max letter = 
    let end_spaces = dup (num_end_spaces max letter) ' ' in 
    let middle_spaces = dup (space_in_between letter) ' ' in 
        end_spaces ++ [letter] ++ middle_spaces ++ [letter] ++ end_spaces

space_in_between :: Char -> Int
space_in_between letter = if letter == 'A' then 0 else (ord letter - ord 'A' ) * 2 -1

num_end_spaces :: Int -> Char -> Int
num_end_spaces max letter =  if letter == 'A' then (max - 1) `quot` 2 else (max - (space_in_between letter) - 2) `quot` 2

dup :: Int -> Char -> String 
dup count letter = map (\b -> letter) [1..count]