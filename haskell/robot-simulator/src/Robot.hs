module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot { coordinates :: (Integer, Integer),
                    bearing :: Bearing }
        deriving (Eq, Show)

-- bearing :: Robot -> Bearing
-- bearing robot = error "You need to implement this function."

-- coordinates :: Robot -> (Integer, Integer)
-- coordinates robot = error "You need to implement this function."

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot coordinates direction

run :: Robot -> Char -> Robot
run robot 'L' = Robot (coordinates robot) (turnLeft (bearing robot))
run robot 'A' =
  let (x, y) = coordinates robot in
  case bearing robot of
  North -> Robot (x, y+1) North
  East -> Robot (x+1, y) East
  South -> Robot (x, y-1) South
  West -> Robot (x-1, y) West
run robot 'R' = Robot (coordinates robot) (turnRight (bearing robot))
run robot _ = robot


simulate :: Robot -> String -> Robot
simulate robot instructions =
  foldl (\acc instruction -> run acc instruction ) robot instructions

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North