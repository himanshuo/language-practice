module DNA (nucleotideCounts) where

import Data.Map

-- when using Either, convention is that Left is error, Right is desired value

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts "" = Right $ fromList [('A',0), ('C',0), ('G', 0), ('T', 0)]
nucleotideCounts (x:xs) = 
    let countsForRest = nucleotideCounts xs in
    case countsForRest of 
        Left e -> Left e
        Right mapForRest -> 
            let currentVal = Data.Map.lookup x mapForRest in 
            case currentVal of 
                Nothing -> Left "Invalid DNS Strand"
                Just oldCount -> 
                    let updatedMap = insert x (oldCount+1) mapForRest in 
                        Right updatedMap