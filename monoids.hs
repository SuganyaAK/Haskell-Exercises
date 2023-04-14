
newtype Day = Day Int
  deriving (Show)

newtype Month = Month Int
  deriving (Show)

f:: Day -> Month -> String
f (Day day) (Month month) = "Day : " ++ show day ++ "Month : " ++ show month

newtype Sum a = Sum a
  deriving (Show,Eq,Ord,Num)

getSum :: Sum a -> a
getSum (Sum x) = x 

instance Num a => Semigroup (Sum a) where
  x <> y = x+y

instance Num a => Monoid (Sum a) where
 mempty = Sum 0

newtype Product a = Product a
  deriving (Show,Eq,Ord,Num)

getProduct :: Product a -> a
getProduct (Product x) = x 

instance Num a => Semigroup (Product a) where
  x <> y = x*y

instance Num a => Monoid (Product a) where
 mempty = Product 1
 --mappend = (*)

lst :: [Int]
lst = [1,2,3,4,5,6,7,8]

data Pair a b = MkPair a b

safeHead :: [a] -> Either String a
safeHead xs = if null xs then Left "error: Empty List" else Right (head xs)  
--safeHead xs = if null xs then Nothing else Right (head xs)  
--safeHead xs = if null xs then Left else Right (head xs)  

--data Maybe a = Nothing | Just a

data Person = MkPerson String Int

data Pokemon = MkPokemon { getName :: String, getNumber :: Int }
 deriving Show