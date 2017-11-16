module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = isLeapYearInternal (rem year 4 == 0) (rem year 100 == 0) (rem year 400 == 0) 

isLeapYearInternal :: Bool -> Bool -> Bool -> Bool
isLeapYearInternal True False False = True 
isLeapYearInternal True _ True = True 
isLeapYearInternal _ _ _ = False

    -- | True False False = True 
    -- | True _ True      = True 
    -- | otherwise        = False

    -- on every year that is evenly divisible by 4
    -- except every year that is evenly divisible by 100
    --   unless the year is also evenly divisible by 400
  

    -- divByFour divByHundred divByFourHundred   RESULT
    -- 0          0              0                 0
    -- 0          0              1                 0
    -- 0          1              0                 0
    -- 1          0              0                 1
    -- 0          1              1                 0
    -- 1          1              0                 0
    -- 1          0              1                 1
    -- 1          1              1                 1
    

    -- numOfRealSolutions a b c
    -- | disc > 0  = 2
    -- | disc == 0 = 1
    -- | otherwise = 0
    --     where
    --     disc = b^2 - 4*a*c



    -- isLeapYear year
    -- | year == 1 = True 
    -- | otherwise = False


    -- f :: Int -> Int
    -- f 0 = 1
    -- f 1 = 5
    -- f 2 = 2
    -- f _ = -1
    