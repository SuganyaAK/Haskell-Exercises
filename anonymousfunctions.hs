greaterThan100 :: [Integer] -> [Integer]
greaterThan100 list = filter grt list

grt:: Integer -> Bool
grt int = int>100

-- using anonymous function

greaterThan100' :: [Integer] -> [Integer]
greaterThan100' list = filter (\x-> x>100) list

---- multiply three numbers

mul3 :: Integer->Integer->Integer->Integer
mul3 x y z = x*y*z

--- Function composition

foo :: (b->c)-> (a->b)-> (a->c)
foo f g = \x -> f(g x)
-- type of f = b -> C
-- type of g = a-> b
-- type of x = a
-- to get the output (a->c)  we can  move from a->b> c

howManyNumbers ::[Integer]-> Int
howManyNumbers list = length (greaterThan100 list)

howManyNumbers' :: [Integer] -> Int
howManyNumbers' = length.greaterThan100

howManyNumbersMore5 :: [Integer] ->Bool
howManyNumbersMore5 = (>5).length. greaterThan100

--computation ::[Int] -> Int
--computation [] = 0
--composition (x:xs)
--    | x>3 = 7*x+2 + foobar xs
--    |otherwise = foobar xs

computation' :: [Int] -> Int
computation' = sum.map (\x->7*x+2).filter (>3)

curry' :: ((a->b)->c)-> a->b->c 
curry' = undefined

uncurry' = (a->b->c) -> (a,b)->c
uncurry'= undefined

fold:: b-> (a->b->b)->[a]-> b 
fold z f [] = z
fold z f (x:xs) = f x ( fold z f xs)