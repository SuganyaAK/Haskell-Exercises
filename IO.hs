myReceipe :: IO ()
myReceipe = do 
 putStr "Enter Number :"
 num <- readLn :: IO Int
 putStr "The output is : "
 putStrLn (show (num+1))
 
myReceipe1 :: IO Int
myReceipe1 = do
 putStr "Enter input : "
 num <- readLn :: IO Int
 putStr "The output is :"
 putStrLn (show (num+1))
 return (num+1)

myReceipe2 :: IO Int
myReceipe2 = do
 putStr "Enter input : "
 num <- readLn :: IO Int
 return num
 putStr "The output is :"
 putStrLn (show (num+1))
 return (num+1)


getLargeNumber :: IO Int
getLargeNumber = do
 putStr "Enter number : "
 num <- readLn :: IO Int
 if num < 5
  then do 
   putStr "Number is too small,try again"
   getLargeNumber
 else do 
   return num
    
getLargeNumber1 :: IO ()
getLargeNumber1 = do
 putStr "Enter number : "
 num1 <- readLn :: IO Int
 putStr "Enter number : "
 num2 <- readLn :: IO Int
 let sum = num1+num2
 if sum >=10
  then do 
   putStr $ "The output is :" ++ show sum
 else do 
   putStr "Sum is less than 10 , Enter another number :"
   num3 <- readLn :: IO Int
   putStr $ "The output is : " ++ show (sum+num3)
