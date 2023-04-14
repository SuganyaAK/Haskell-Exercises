data Color = Red
           | Green
           | Pink
           | Blue
  deriving Show 

isFavoriteColor :: Color -> Bool
isFavoriteColor Pink = True
isFavoriteColor _ = False

data FailableDouble = Ok Double 
                    | Failure
 deriving Show


mySafeDiv :: Double -> Double -> FailableDouble
mySafeDiv _ 0 = Failure
mySafeDiv  x y = Ok (x/y)

myComputation :: Double ->Double ->Double -> FailableDouble
myComputation x y z = myComputationAdd x (mySafeDiv y z)


myComputationAdd :: Double -> FailableDouble ->FailableDouble
myComputationAdd m (Ok d)= Ok (m+d)
myComputationAdd m Failure = Failure


patternMatching :: [[Int]] -> String
patternMatching [[],[],[]] = "No value"
patternMatching [[z],[_,x,xs],[_,y]] = "z:" ++ show z ++ "Y:"++ show y ++ "X:" ++ show xs  

data Person = Person String Integer Bool
    deriving Show

getAge :: Person -> Integer
getAge (Person name age vegan) = age


data PersonorColor = POCPerson Person
                   | POCColor Color
 deriving Show

personNameorColorName :: PersonorColor ->String
personNameorColorName (POCPerson (Person name age vegan)) = name
personNameorColorName (POCColor Red) = "Red"
personNameorColorName (POCColor Green) = "Green"


personNameorColorName2 :: PersonorColor ->String
personNameorColorName2 (POCPerson (Person name age vegan)) = name
personNameorColorName2 (POCColor color ) = show color


whatIsMyName :: Person -> String
whatIsMyName (Person name age vegan) = "I am  " ++ show (Person name age vegan) ++  " and my name is   " ++ name

whatIsMyName2 :: Person -> String
whatIsMyName2 p@(Person name age vegan) = "I am  " ++ show p ++  " and my name is   " ++ name


foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  |  otherwise = 4
foo n 
  |  n < 0 = 0
  |  n `mod` 17 == 2 = -43
  |  otherwise  = n+3 