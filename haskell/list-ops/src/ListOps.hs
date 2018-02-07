module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )


--foldl (\acc x -> something that outputs next acc value) original_acc list_to_iterate_over
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) =
  let strictlyEvalulatedVal = f z x in
    strictlyEvalulatedVal `seq` (foldl' f strictlyEvalulatedVal xs)

--foldr (\x acc -> updated_acc) acc iter_list -> output
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) =
  let resultOfRest = foldr f z xs in
    resultOfRest `seq` f x resultOfRest

length :: [a] -> Int
length xs = lengthInternal xs 0

lengthInternal :: [a] -> Int -> Int
lengthInternal [] cur = cur
lengthInternal (x:xs) cur = lengthInternal xs (cur+1)

reverse :: [a] -> [a]
reverse xs = reverseInternal xs []

reverseInternal :: [a] -> [a] -> [a]
reverseInternal ([]) newList = newList
reverseInternal (o:oldList) newList = (reverseInternal oldList newList) ++ [o]

map :: (a -> b) -> [a] -> [b]
map f ([]) = []
map f (x:xs) = (f x):(map f xs)

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = foldl (\acc x -> if p x then acc ++ [x] else acc) [] xs

(++) :: [a] -> [a] -> [a]
([]) ++ ys = ys
(x:[]) ++ ys = x:ys
(x:xs) ++ ys = x:(xs ++ ys)

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) =
  let resultOfRest = concat xs in
  resultOfRest `seq` (x ++ resultOfRest)
