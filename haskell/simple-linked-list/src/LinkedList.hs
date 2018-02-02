module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = LinkedList { datum :: a, next :: LinkedList a}
                    | Nil
                    deriving (Eq, Show)


-- datum :: LinkedList a -> a
-- datum linkedList = error "You need to implement this function."

fromList :: [a] -> LinkedList a
fromList xs = foldr (\x acc -> LinkedList x acc) nil xs

isNil :: LinkedList a -> Bool
isNil linkedList = case linkedList of
                        LinkedList _ _-> False
                        Nil -> True

new :: a -> LinkedList a -> LinkedList a
new x linkedList = LinkedList x linkedList

-- next :: LinkedList a -> LinkedList a
-- next linkedList = error "You need to implement this function."

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = reverseInternal linkedList nil

reverseInternal :: LinkedList a -> LinkedList a -> LinkedList a
reverseInternal oldList newList = case oldList of
                                    Nil -> newList
                                    LinkedList val nextNode -> reverseInternal nextNode (LinkedList val newList)

toList :: LinkedList a -> [a]
toList linkedList = case linkedList of
                          Nil -> []
                          LinkedList datum next -> [datum] ++ (toList next)
