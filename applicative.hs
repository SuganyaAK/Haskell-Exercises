-- applicative are used in failure cases
-- functors are weak , they have only map function
-- functor = (a->b)
-- applicative = combines a and b and gives c -- used for combing and b to give c
import Language.Haskell.TH (NameSpace)
import Data.Text.Array (run)
type Name = String
type PhoneNumber = String

data Employee = Employee Name PhoneNumber
 deriving Show

maybeEmployee :: Maybe Name -> Maybe PhoneNumber -> Maybe Employee
maybeEmployee (Just na) (Just num) = Just( Employee na num)
maybeEmployee _ _ = Nothing

-- instead of joining them like this we can use applicative functor
-- below is the definition of applicative

--class Functor f => Applicative f where
--     pure :: a -> f a 
--     (<*>) :: f(a->b) -> f a -> f b

liftA2 :: Applicative f => (a->b->c) -> f a -> f b -> f c
liftA2 h x y = let fab = fmap h x
                 in fab <*> y

liftA3 :: Applicative f => (a->b->c->d)-> f a-> f b -> f c -> f d 
liftA3 h fa fb fc = let fab = fmap h fa
                        fbc = fab <*> fb 
                    in fbc <*> fc

-- Parser using applicative
-- to create an applciative we need to create a new type    

newtype Parser a = Parser (String-> Maybe (String, a))

runParser :: Parser a ->String -> Maybe (String,a)
runParser (Parser f) inputString = f inputString 

satisfy :: (Char->Bool) -> Parser Char
satisfy condition = Parser f
 where f :: String -> Maybe(String, Char)
       f(c:cs)
        | condition c = Just (cs,c)
        | otherwise = Nothing 
       f[] =Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

satisfyMany :: (Char -> Bool) -> Parser String
satisfyMany condition = Parser f
 where f :: String -> Maybe (String,String)
       f input = case span condition input of
        ([],_) -> Nothing
        (parsed, remaining) -> Just(remaining, parsed) 
    
instance Functor Parser where
    fmap fab p = Parser f
     where 
        f inputString = case runParser p inputString of
            Nothing -> Nothing
            Just (s,x) -> Just (s,fab x)

instance Applicative Parser where
    pure x = Parser f 
     where
        f s = Just (s,x)
    pab <*> pa = Parser f 
     where 
        f inputString = case runParser pab inputString of
            Nothing -> Nothing
            Just (s',fab) -> case runParser pa s' of 
                Nothing -> Nothing
                Just(s', a) -> Just (s',fab a)  

tillComma :: Parser String
tillComma = satisfyMany (/= ',')

comma :: Parser Char
comma = char ','

employeeParser :: Parser Employee
employeeParser = liftA3 (\name comma phone-> Employee name phone) tillComma comma tillComma


halfIfEven :: Integer -> Maybe Integer 
halfIfEven x = if even x
                then Just x
                    else Nothing
