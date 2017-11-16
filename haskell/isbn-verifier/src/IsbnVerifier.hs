module IsbnVerifier (isbn) where
import Data.Char

isbn :: String -> Bool
isbn str = validate_isbn (str_to_int_arr str)

-- Valid Strings:
--          3-598-21507
--          3598215088
--          3-598-21507-X

-- Invalid Strings:
--          3-598-21507-XX

str_to_int_arr :: String -> [Int]
str_to_int_arr ('X':[]) = [10] 
str_to_int_arr (x:[]) = if isNumber x 
                        then [Data.Char.digitToInt x]
                        else []
str_to_int_arr ('-':xs) = str_to_int_arr(xs)                           
str_to_int_arr (x:xs) = if isNumber(x) 
                        then [Data.Char.digitToInt x] ++ str_to_int_arr(xs)
                        else []
                        
                            -- [int_value] + str_to_int_arr(xs)
                            -- else str_to_int_arr(xs)
                        

-- dropThree :: [a] -> [a]
-- dropThree (_:_:_:xs) = xs
-- dropThree _          = []


-- do result <- runExceptionalT (readText "test")
-- case result of
--    Exception e -> putStrLn ("When reading file 'test' we encountered exception " ++ show e)
--    Success x -> putStrLn ("Content of the file 'test'\n" ++ x)


validate_isbn :: [Int] -> Bool 
validate_isbn (x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:[]) = rem (x1 * 10 + x2 * 9 + x3 * 8 + x4 * 7 + x5 * 6 + x6 * 5 + x7 * 4 + x8 * 3 + x9 * 2 + x10 * 1) 11 == 0
validate_isbn sx = False 