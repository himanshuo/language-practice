module Beer (song) where

song :: String
song = 
    let addVerse = \acc num -> acc ++ (genVerse num) in 
    let repeatablePart = foldl addVerse "" [99,98..4] in 
        repeatablePart ++ "3 bottles of beer on the wall, 3 bottles of beer.\n\
        \Take one down and pass it around, 2 bottles of beer on the wall.\n\
        \\n\
        \2 bottles of beer on the wall, 2 bottles of beer.\n\
        \Take one down and pass it around, 1 bottle of beer on the wall.\n\
        \\n\
        \1 bottle of beer on the wall, 1 bottle of beer.\n\
        \Take it down and pass it around, no more bottles of beer on the wall.\n\
        \\n\
        \No more bottles of beer on the wall, no more bottles of beer.\n\
        \Go to the store and buy some more, 99 bottles of beer on the wall.\n"

genVerse :: Int -> String
genVerse num = 
    let space = if num == 99 then "" else " " in
    -- space ++
    show num ++
    " bottles of beer on the wall, " ++
    show num ++
    -- space ++
    " bottles of beer.\nTake one down and pass it around, " ++
    show (num-1) ++
    -- space ++
    " bottles of beer on the wall.\n\n"