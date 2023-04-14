import Control.Applicative
data Person = Person String Int
 deriving Show

data Policy = Policy String Int
 deriving Show

getPolicyAge :: Policy -> Int
getPolicyAge (Policy _ age) = age

getPolicyName :: Policy -> String
getPolicyName (Policy name _) = name

isFamous :: Policy -> String -> Bool
isFamous ( Policy lastName _) actualName = actualName == lastName

getsDiscount :: Policy -> Int -> Bool
getsDiscount (Policy _ maxAge) age = age < maxAge

interesting :: Policy -> Person -> Bool
interesting policy (Person actualName age) = isFamous policy actualName || getsDiscount policy age

-- READER monad
newtype Reader r a = Reader (r -> a)

runReader :: Reader r a -> r -> a
runReader (Reader f) env = f env

isFamousR :: Reader Policy (String -> Bool)
-- r is Policy
-- a is String -> Bool
-- r -> a is Policy -> String -> Bool
isFamousR = Reader isFamous

isFamousR' :: String -> Reader Policy Bool
isFamousR' actualName = Reader (\policy -> isFamous policy actualName)

getsDiscountR :: Int -> Reader Policy Bool
getsDiscountR age = Reader (\policy -> getsDiscount policy age)

instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap g (Reader fra) = Reader (\r -> g (fra r))
  -- fmap g (Reader fra) = Reader (g . fra)

discountMessage :: Bool -> String
discountMessage True = "You are eligible for discount"
discountMessage False = "You are too old, sorry."

discountMessageR :: Int -> Reader Policy String
discountMessageR age = fmap discountMessage (getsDiscountR age)

instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  pure x = Reader (\_ -> x)

  -- <*> :: Reader r (a -> b) -> Reader r a -> Reader r b
  readerAB <*> readerA = Reader (\r -> (runReader readerAB r) (runReader readerA r))

isInterestingR :: Person -> Reader Policy Bool
isInterestingR (Person name age) = let famousReader = isFamousR' name
                                       discountReader = getsDiscountR age   
                                    in liftA2 (||) famousReader discountReader

betterDiscountMessage :: Bool -> Reader Policy String
betterDiscountMessage True = Reader (\(Policy _ age) -> "Hurray you get a discount because you are younger than " ++ (show age))
betterDiscountMessage False = Reader (\(Policy _ age) -> "Sorry you don't get a discount because you are older than " ++ (show age))

instance Monad (Reader r) where
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  readerA >>= next = Reader f
    where
   -- f :: r -> b
      f r = let a = runReader readerA r
                readerB = next a
             in runReader readerB r

ask :: Reader r r
ask = Reader (\r -> r)

asks :: (r -> a) -> Reader r a
asks f = Reader f

betterDiscountMessageR :: Int -> Reader Policy String
betterDiscountMessageR age = (getsDiscountR age) >>= betterDiscountMessage

betterDiscountMessageRUsingDo :: Int -> Reader Policy String
betterDiscountMessageRUsingDo age = do
  didWeGetDiscount <- getsDiscountR age
  betterDiscountMessage didWeGetDiscount

askExample :: Int -> Reader Policy String
askExample age = do
  didWeGetDiscount <- getsDiscountR age
  (Policy _ maxAge) <- ask
  if didWeGetDiscount then return ("hurray you got discount because less than maxAge: " ++ show maxAge)
                      else return ("sorry you don't get discount because more than maxAge:" ++ show maxAge)

askExample2 :: Int -> Reader Policy String
askExample2 age = do
  (Policy _ maxAge) <- ask
  if age < maxAge then return ("hurray you got discount because less than maxAge: " ++ show maxAge)
                      else return ("sorry you don't get discount because more than maxAge:" ++ show maxAge)

asksExample3 :: Int -> Reader Policy String
asksExample3 age = do
  maxAge <- asks getPolicyAge
  if age < maxAge then return ("hurray you got discount because less than maxAge: " ++ show maxAge)
                      else return ("sorry you don't get discount because more than maxAge:" ++ show maxAge)

asksExample4 :: Reader Policy String
asksExample4 = do
  policyNameTooLong <- asks ((>25) . length . getPolicyName)
  if policyNameTooLong then return "policy name is too long, who even has such a name"
                       else return "normal policy name"

isMagicNumber :: Int -> Reader [Int] Bool
isMagicNumber myNumber = do
  asks (myNumber `elem`)
