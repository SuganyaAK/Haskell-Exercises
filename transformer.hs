import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class(lift)
import Text.Read (readMaybe)
import Data.Functor.Classes


readLineWithoutTran :: Read a => IO (Maybe a)
readLineWithoutTran = do
    input <-getLine
    return (readMaybe input)

readLine :: Read a => MaybeT IO a
readLine = do
    input <- lift getLine
    MaybeT $ return $ readMaybe input

sumOfUserInputs :: MaybeT IO Int
sumOfUserInputs = do
   num1 <- readLine
   num2 <- readLine
   return (num1+num2)