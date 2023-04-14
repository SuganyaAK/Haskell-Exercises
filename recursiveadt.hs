data IntList = Empty 
             | Cons Int IntList
  deriving Show

list0 = Empty
list1 = Cons 1 list0
list2 = Cons 2 list1

lengthIntList :: IntList -> Integer
lengthIntList Empty = 0
lengthIntList (Cons x xs) = 1+ lengthIntList xs

prodIntList :: IntList -> Int
prodIntList Empty = 1
prodIntList (Cons x xs) = x * prodIntList xs


data Tree = Leaf
          | Node Tree Int Tree
  deriving Show

dummyTree :: Tree
dummyTree = Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 (Node Leaf 42 Leaf))

existsInTree :: Int ->Tree -> Bool
existsInTree x Leaf = False
existsInTree x (Node left y right) = (x==y) || existsInTree x left || existsInTree x right


prodInTree :: Tree -> Int
prodInTree Leaf = 1
prodInTree (Node left y right) = y * prodInTree left * prodInTree right


largestInTree :: Tree -> Int
largestInTree Leaf =0
largestInTree (Node left x right) = let largestInLeft = largestInTree left 
                                        largestInRight = largestInTree right
                                    in max x (max largestInLeft largestInRight)