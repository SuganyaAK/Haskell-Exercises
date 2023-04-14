areBothEmpty :: [Int]-> [Int] -> Bool
areBothEmpty list1 list2 = case (list1,list2) of 
 ( [],[] ) -> True
 _         -> False


myInsertAt :: Integer-> Integer ->[Integer] -> [Integer]
myInsertAt z pos []= error " Impossible, pos should be less than length of the list"
myInsertAt z 0 xs  = z:xs
myInsertAt z pos (x:xs) = x : myInsertAt z (pos-1)xs
  