module DNA (toRNA) where

-- DNA -> RNA
-- `G` -> `C`
-- `C` -> `G`
-- `T` -> `A`
-- `A` -> `U`

toRNA :: String -> Maybe String
toRNA ([]) = Just ""
toRNA (x:xs) = 
    let match = toRNASingle x in 
        case match of 
            Just m -> 
                let rest = toRNA xs in 
                    case rest of 
                        Just rest_str -> Just (m:rest_str)
                        Nothing -> Nothing 
            Nothing -> Nothing
    
toRNASingle :: Char -> Maybe Char 
toRNASingle 'G' = Just 'C'
toRNASingle 'C' = Just 'G'
toRNASingle 'T' = Just 'A'
toRNASingle 'A' = Just 'U'
toRNASingle _ = Nothing
    