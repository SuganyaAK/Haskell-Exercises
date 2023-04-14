data IntList1 = Empty 
              | Cons Int IntList1
 deriving Show

absAll :: IntList1 -> IntList1
absAll Empty = Empty
absAll (Cons x xs ) = Cons (abs x) (absAll xs)

squareAll :: IntList1 -> IntList1
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

mapIntList :: (Int->Int) -> IntList1 -> IntList1
mapIntList f Empty = Empty
mapIntList f (Cons x xs) = Cons (f x ) (mapIntList f xs) 

absAll' :: IntList1 -> IntList1
absAll' list = mapIntList abs list 

squareAll' :: IntList1 -> IntList1
squareAll' list = mapIntList square list
 where
  square :: Int->Int
  square x = x*x

keepEvens :: IntList1 -> IntList1
keepEvens Empty = Empty
keepEvens (Cons x xs)
 | even x = Cons x (keepEvens xs)
 | otherwise = keepEvens xs
 
lessThanTen :: IntList1 -> IntList1
lessThanTen Empty = Empty
lessThanTen (Cons x xs)
 | x < 10 = Cons x (lessThanTen xs)
 | otherwise = lessThanTen xs 

filterIntList :: (Int -> Bool) -> IntList1 -> IntList1
filterIntList predicate Empty= Empty
filterIntList predicate (Cons x xs) 
 | predicate x = Cons x (filterIntList predicate xs)
 | otherwise = filterIntList predicate xs

keepEvens' :: IntList1 -> IntList1
keepEvens' list = filterIntList even list

lessThanTen' :: IntList1 -> IntList1
lessThanTen' list = filterIntList less10 list
 where
  less10 :: Int -> Bool
  less10 x = x < 10