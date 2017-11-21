module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []
discard f (x:xs) = 
    if f x 
    then discard f xs
    else [x] ++ discard f xs

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep f (x:xs) = 
    if f x 
    then [x] ++ keep f xs
    else keep f xs
