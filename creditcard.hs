dropLastDigitInList :: [Integer]-> [Integer]
dropLastDigitInList [x] =[]
dropLastDigitInList (x:xs) = x: dropLastDigitInList xs

checkDigit :: [Integer] -> Integer
checkDigit [] = 0
checkDigit [x] =x
checkDigit (x:xs) =checkDigit xs

reverseNumbers :: [Integer] -> [Integer]
reverseNumbers [] =[]
reverseNumbers (x:xs) = reverseNumbers xs ++ [x]

multiplyOddPlaces :: [Integer] -> [Integer]
multiplyOddPlaces [] =[]
multiplyOddPlaces [x] = [2*x]
multiplyOddPlaces (x:y:zs) = (2*x) :y :multiplyOddPlaces zs

subtractBy9 :: [Integer] -> [Integer]
subtractBy9 [] = []
subtractBy9 (x:xs)
    | x > 9 = (x-9) : subtractBy9 xs
    | otherwise = x:subtractBy9 xs 

addAllDigits :: [Integer] -> Integer
addAllDigits [] = 0
addAllDigits (x:xs) = x+ addAllDigits xs

isDivisibleBy10 :: Integer -> Bool
isDivisibleBy10 x = mod x 10 ==0

validate :: [Integer] -> Bool
validate card =
    let sumOfAllDigits = addAllDigits (subtractBy9 (multiplyOddPlaces(reverseNumbers (dropLastDigitInList card))))
        sumPlusCheckDigit = sumOfAllDigits + checkDigit card
    in isDivisibleBy10 (sumPlusCheckDigit)

creditCardTest :: [Integer]
creditCardTest = [4,5,5,6,7,3,7,5,8,6,8,9,9,8,5,5]