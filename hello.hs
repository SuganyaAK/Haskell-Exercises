factorialWithGuards :: Integer -> Integer
factorialWithGuards n
  | n == 0    = 1
  | otherwise = n * factorialWithGuards (n-1)

hailStone :: Integer -> Integer
hailStone n
    | mod n 2 == 0 = div n 2
    | otherwise = 3*n+1 

foo :: Int ->Int->Int
foo 0 x
    | x > 50 = 50
    | x > 10 = 10

foo z 10 = z
foo x z  = z + x 

checkElementInPairIsZero :: ( Integer,Integer) -> Bool
checkElementInPairIsZero (0 ,_) = True
checkElementInPairIsZero (_ ,0) = True
checkElementInPairIsZero _ = False

hailStoneSeq :: Integer ->[Integer]
hailStoneSeq n 
    | n == 0 = [0]
    | n == 1 = [1]
    | n > 0 = n : hailStoneSeq (hailStone n) 

hailStoneSeq1 :: Integer ->[Integer]
hailStoneSeq1 1 = [1]
hailStoneSeq1 n = n : hailStoneSeq1 (hailStone n) 

decreasingTo1 :: Integer -> [Integer]
decreasingTo1 n
    | n < 1  = []
    |otherwise = n : decreasingTo1 (n-1) 

inListLength :: [Integer] -> Integer
inListLength [] = 0
inListLength (x:xs) = 1 + inListLength xs


sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo [x] = [x]
sumEveryTwo (x:y:xs) = (x+y) : sumEveryTwo xs

myTake :: Int -> [Integer] -> [Integer]
myTake 0 (x:xs) =[]
myTake 1 (x:xs) = [x]
myTake n []  = []
myTake n (x:xs) = x: myTake (n-1) xs


myTakeClassDiscussion :: Int -> [Integer] ->[Integer]
myTakeClassDiscussion _ [] = []
myTakeClassDiscussion 0 xs = []
myTakeClassDiscussion n (x:xs) = x : myTakeClassDiscussion (n-1) xs


hailStoneSteps :: Integer -> Integer
hailStoneSteps n = inListLength(hailStoneSeq n) -1


largestInList ::  [Integer] ->Integer
largestInList [x] = x 
largestInList (x:xs)
    | x > maxTail =  x 
    | otherwise = maxTail
    where maxTail = largestInList xs 


smallestInList ::  [Integer] ->Integer
smallestInList [x] = x 
smallestInList (x:xs)
    | x < maxTail =  x 
    | otherwise = maxTail
    where maxTail = smallestInList xs 


type String = [Char]

isOdd :: Int -> Bool
isOdd n
 | n `div` 2 == 0 = True
 |otherwise = False

isCheck :: [Char] -> Char
isCheck (x:xs)
 | x == 's' = 'c' 
 | otherwise = 'r' 

(&&!) :: Bool -> Bool -> Bool
True &&! True = True
True &&! False = False 
False &&! True = False 
False &&! False = False 
