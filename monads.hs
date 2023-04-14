import Graphics.Win32 (ResourceImageType)
import Text.XHtml (action)

halfIfEven :: Int -> Maybe Int 
halfIfEven x = if even x
                then Just x
                    else Nothing


-- snakes and ladders

board :: Int -> [Int]
board 10 =[10]
board 13 =[40,31]
board 46 =[3]
board x =[x]

possibleNextPositions :: Int -> [Int]
possibleNextPositions currentPosition = [currentPosition+1.. currentPosition+6] >>= board

possibleNextPositionsDo :: Int -> [Int]
possibleNextPositionsDo currentPosition = do
    diceResult <- [1..6]
    board(currentPosition + diceResult)

halfXPlusOneDo :: Int -> Maybe Int
halfXPlusOneDo x = do
    xbyTwo <- halfIfEven x
    Just (xbyTwo+1)

halfXPlusOneBind :: Int -> Maybe Int 
halfXPlusOneBind x = halfIfEven x >>= (\y->Just (y+1))


sequenceM :: Monad m => [m a] -> m[a]
sequenceM [] = return []
sequenceM (action :actions) = do
    x <- action
    xs <- sequenceM actions 
    return (x:xs)

check :: Int -> IO Bool
check value = if value < 5
               then do
                 putStrLn "Value too small"
                 return False
               else do 
                 putStrLn " VAlue Great!"
                 return True

readInt :: IO Int
readInt = readLn

readAndCheckBind :: IO Bool
readAndCheckBind = readInt>>= check

readAndCheckDo :: IO Bool
readAndCheckDo = do
    num <-readInt
    check num

doExample1 :: IO ()
doExample1 = do
    num <-readInt
    putStrLn $ show $ num+1

bindExample1 :: IO ()
bindExample1 = readInt >>= (\num -> putStrLn $ show $ num+1)

doExample2 :: IO()
doExample2 = do
    num1<-readInt
    num2<-readInt
    putStrLn $ show $ num1+num2

bindExample2 :: IO ()
bindExample2 = readInt >>= (\num1 -> readInt >>= (\num2->putStrLn $ show $ num1+num2))

replicateM :: Monad m => Int -> m a -> m [a]
replicateM 0 _ = return []
replicateM n action = do
    x <- action
    xs <- replicateM (n-1) action
    return (x:xs) 

replicateMBind :: Monad m => Int -> m a -> m [a]
replicateMBind 0 _ = return []
replicateMBind n action = action >>= (\xs -> replicateMBind (n-1) action)

replicateM1 :: Monad m => Int -> m a -> m [a]
replicateM1 n action = sequenceM $ replicate n action 


(>>$) :: Monad m  => m a -> m b -> m b
x >>$ y = x >>= (\_ -> y)

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1,x+2]

exA1 = [10,20,30] >>= addOneOrTwo