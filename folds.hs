import Data.Char

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x+ sum' xs


product' :: [Int] -> Int
product' [] = 1 
product' (x:xs) = x * product' xs


length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

--- here fold uses only a list not on all types of data structure
-- common points 
-- base case has default result
-- combines the current value with elements of list

fold' :: b -> (a->b->b) -> [a]-> b
fold' z f [] = z
fold' z f (x:xs) = f x (fold' z f xs)

sumUsingFold :: [Int] -> Int
sumUsingFold xs = fold' 0 (+) xs

productUsingFold :: [Int] -> Int
productUsingFold xs = fold' 1 (*) xs

lengthUsingFold :: [a] -> Int
lengthUsingFold xs = fold' 0 (\_ acc -> 1+acc) xs


andUsingFold :: [Bool] -> Bool
andUsingFold xs = fold' True (&&) xs

orUsingFold :: [Bool] -> Bool
orUsingFold xs = fold' False (||) xs


anyUsingFold :: (a->Bool) -> [a] -> Bool
anyUsingFold f xs = fold' False (\x acc -> f x || acc) xs 

allUsingFold :: (a->Bool)->[a] -> Bool
allUsingFold f xs = fold' True (\x acc -> f x && acc) xs

mapUsingFold :: (a->b) -> [a] -> [b]
mapUsingFold f xs = fold' [] (\x acc -> f x :acc) xs

filterUsingFold :: (a->Bool) -> [a] -> [a]
filterUsingFold f xs = fold' [] (\x acc -> if f x == False then acc else x:acc) xs

reverse' :: [a] -> [a]
reverse' xs = fold' [] (\x acc -> acc ++ [x]) xs

elemIn :: Int -> [Int] -> Bool
elemIn a xs = fold' False (\x acc -> a==x||acc) xs


elemIn' ::Eq a => a -> [a] -> Bool
elemIn' elem xs = fold' False (\x acc -> elem==x||acc) xs

foo :: [String]
foo = map (\x->x)["vulpix","fydgrhdb"]
