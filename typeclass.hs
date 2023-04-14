data Foo = F Int | G Char
 deriving Eq


--class Eq1 a where 
-- (==) :: a -> a -> Bool
-- (/=) :: a -> a -> Bool

-- Here we are making Foo a member of Eq typeclass

--instance Eq1 Foo where
-- (F x1) == (F x2) = x1 == x2
-- (G c1) == (G c2) = c1 == c2
--  _ == _ = False
--We have to create our own typeclass
-- toList 1= [1]
-- toList False =[0]
-- toList (42,False) = [42,0]

class Listable a where
 toList :: a->[Int]

instance Listable Int where
 toList x = [x]

instance Listable Bool where
 toList False =[0]
 toList True = [1]

instance Listable [Int] where
 toList x = x

instance (Listable a ,Listable b)=> Listable (a,b) where 
 toList (x,y) = toList x ++ toList y

sumL :: Listable a => a -> [Int]
sumL x = toList x

data TreeInt = NodeInt TreeInt Int TreeInt | LeafInt

data Tree a = Node (Tree a) a (Tree a) |Leaf 

instance Listable a =>Listable (Tree a) where
 toList Leaf =[]
 toList (Node left c right) = toList left ++ toList c ++ toList right