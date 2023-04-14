import Control.Monad.Trans.Writer(Writer, tell,runWriter)
import Data.Monoid ( Sum(Sum, getSum) )

func1 :: String -> (Int, String)
func1 inputString = if length inputString `mod` 2 == 0
  then func2 (0, inputString)
  else func3 (0, tail inputString)

func2 :: (Int, String) -> (Int, String)
func2 (prev, inputString) = if length inputString > 10
  then func3 (prev + 1, take 9 inputString)
  else (prev + 10, inputString)

func3 :: (Int,String) -> (Int,String)
func3 (prev,inputString) = if length inputString < 10
    then (prev+length inputString,inputString++inputString)
    else (prev+5, take 5 inputString)

writer3 :: String -> Writer (Sum Int) String
writer3 inputString = if length inputString <10
    then do 
        tell (Sum $ length inputString)
        return (inputString ++ inputString)
    else do
        tell (Sum 5)
        writer3 (take 5 inputString)
 
writer2 :: String -> Writer (Sum Int) String
writer2 inputString = if length inputString > 10
     then do 
        tell (Sum 1)
        return (take 9 inputString)
     else do
        tell (Sum 10)
        return inputString

writer1 :: String -> (String, Int)
writer1 inputString = if length inputString `mod` 2 == 0
    then let (s,sum) = runWriter (writer2 inputString) in (s, getSum sum)
    else let (s,sum) = runWriter (writer3 (tail inputString)) in (s, getSum sum)


factorialWithLogging :: Int -> Writer String Int
factorialWithLogging 1 = do 
    tell ("We got a 1, the result of the dactorail is 1\n")
    return 1
factorialWithLogging n = do
    tell ("n=" ++ show n ++ ", we will recursively call factorial\n")
    result <-factorialWithLogging (n-1)
    tell ("The result of the computation is " ++ show n)
    return (n *result)