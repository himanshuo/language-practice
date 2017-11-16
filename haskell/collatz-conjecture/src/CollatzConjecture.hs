module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz num
    | num <= 0 = Nothing 
    | otherwise = Just (collatzInternal num 0)

collatzInternal :: Integer -> Integer -> Integer
collatzInternal num count
    | num == 1 = count 
    | (rem num 2) == 0 = collatzInternal (num `div` 2) (count+1)
    | (rem num 2) == 1 = collatzInternal (3 * num + 1) (count+1)


-- bmiTell bmi  
-- | bmi <= 18.5 = "You're underweight, you emo, you!"  
-- | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
-- | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
-- | otherwise   = "You're a whale, congratulations!" 
