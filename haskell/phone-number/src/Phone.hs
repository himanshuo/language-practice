module Phone (number) where
import Data.Char 

-- (NXX)-NXX-XXXX where N is 2-9, X is 1-9
number :: String -> Maybe String
number xs =
    let nums = filter (\x -> isDigit x) xs in 
    let nums' = if invalid nums 0 then drop 1 nums else nums in 
    if (invalid nums 3) || (length nums' /= 10) then Nothing 
    else Just nums' 

invalid :: String -> Int -> Bool
invalid list index = ((list !! index) == '0') || ((list !! index) == '1')
