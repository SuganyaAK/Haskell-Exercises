data List t = E | C t (List t)
 deriving Show

list1 :: List Int
list1 = C 3 (C 5 ( C 1 E))

list2 :: List Char 
list2 = C 'a' (C 'b' ( C 'd' E))

list3 :: List String
list3 = C "Sugan" (C "Arun" ( C "Rithvi" (C "Thak" E)))

filterList :: ( t-> Bool) -> List t -> List t 
filterList predicate E = E 
filterList predicate (C x xs) 
  | predicate x = C x (filterList predicate xs)
  | otherwise = filterList predicate xs


stringMoreThanTwo :: List String -> List String
stringMoreThanTwo list = filterList moreTwo list
 where 
 moreTwo :: String -> Bool
 moreTwo str = length(str) > 2

mapList :: (a -> b) -> List a -> List b
mapList _ E = E 
mapList f ( C x xs )  = C (f x ) (mapList f xs)

headOfTheString :: List String -> List Char
headOfTheString list = mapList head list


filterNonEmpty' :: List String -> List String
filterNonEmpty' list = filterList f list
 where 
  f :: String -> Bool
  f str = length(str)>0

filterNonEmpty :: List (List a) -> List (List a)
filterNonEmpty list = filterList f list
 where 
  f :: List a -> Bool
  f E = False
  f (C _ _) = True

stringMoreThanTwo' :: [String] -> [String]
stringMoreThanTwo' strlist  = filter func strlist
 where 
 func :: String -> Bool
 func str = length(str) > 2

headOfTheString' :: [String]->[Char]
headOfTheString' str = map head str

filterNonEmptyWithHaskellString :: [String] -> [String]
filterNonEmptyWithHaskellString list = filter f list
 where 
  f :: String -> Bool
  f str = length(str)>0

filterNonEmptyListOfList :: [[a]] -> [[a]]
filterNonEmptyListOfList list = filter f list
 where 
  f :: [a] -> Bool
  f [] = False
  f [_] = True