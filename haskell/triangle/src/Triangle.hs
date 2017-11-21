module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Eq a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
    | invalidTriangle a b c = Illegal
    | a==b && b==c = Equilateral
    | a==b || b==c || a==c = Isosceles
    | otherwise = Scalene


invalidTriangle :: (Num a, Eq a, Ord a) => a -> a -> a -> Bool
invalidTriangle a b c
    | a <= 0 || b<=0 || c<=0 = True 
    | a+b < c = True 
    | a+c < b = True 
    | c+b < a = True
    | otherwise = False  
    