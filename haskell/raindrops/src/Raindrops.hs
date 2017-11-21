module Raindrops (convert) where

-- convert :: Int -> String
-- convert n = 
--     let output = "" in 
--     let output' = if rem n 3 == 0 then output ++ "Pling" else output in 
--     let output'' = if rem n 5 == 0 then output' ++ "Plang" else output' in 
--     let output''' = if rem n 7 == 0 then output'' ++ "Plong" else output'' in
--         if output''' == "" 
--         then show n 
--         else output''' 

        

convert :: Int -> String
convert n = 
    let dataList = [(3, "Pling"), (5, "Plang"), (7, "Plong")] in
    let addIfFactor = \acc (num, str) -> if rem n num == 0 then acc ++ str else acc in
    let result = foldl addIfFactor "" dataList in 
        if result == ""
        then show n 
        else result

