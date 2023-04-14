--class functor f  where
-- fmap :: (a->b)-> f a -> f b

import Data.List()
import Data.Text.Lazy (splitOn, Text)

addOneToMaybe :: Maybe Int -> Maybe Int
addOneToMaybe  = fmap (+1) 

makePairMaybe :: Maybe Int -> Maybe (Int,Int)
makePairMaybe m = fmap (\x-> (x,x)) m

lengthF ::Functor f=> f String -> f Int
lengthF = fmap length

newtype USD = USD Float
 deriving (Show,Num)

newtype BTC = BTC Float
 deriving (Show,Num)

convertUSDToBTC :: USD -> BTC
convertUSDToBTC (USD x) = BTC (1000*x)

data Transaction a = Transaction String a
 deriving Show

transaction1 :: Transaction USD
transaction1 = Transaction "txn -18" (USD 10.0)

transaction2 :: Transaction BTC
transaction2 = Transaction "txn -19" (BTC 10.0)

instance Functor Transaction where
    fmap f (Transaction txnid x) = Transaction txnid (f x)

getValue :: Transaction a -> a
getValue (Transaction _ value) = value

data Shape = Triangle Float Float Float
 deriving Show

twiceAsLarge :: Shape -> Shape
twiceAsLarge (Triangle a b c)= Triangle (2*a)(2*b)(2*c)

data ShapeF a = TriangleF a a a
               | SquareF a
 deriving Show

instance Functor ShapeF where
 fmap f (TriangleF x y z) = TriangleF (f x) (f y) (f z)
 fmap f (SquareF x)= SquareF (f x)

twiceAsLarge' :: Num a =>ShapeF a -> ShapeF a
twiceAsLarge' = fmap (*2) 

data Employee = Employee {name :: String, number :: String}
 deriving Show

m_name1,m_name2 :: Maybe String
m_name1 = Nothing
m_name2 = Just "Brent"

m_num1,m_num2 :: Maybe String
m_num1 = Nothing
m_num2 = Just "1234"

exA = Employee <$> m_name1 <*> m_num1