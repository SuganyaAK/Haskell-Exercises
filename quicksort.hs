

partitionLess :: Integer -> [Integer] ->[Integer]
partitionLess pivot [] = []
partitionLess pivot (x:xs)
    | x <= pivot = x: partitionLess pivot xs
    | otherwise = partitionLess pivot xs


partitionGreater :: Integer -> [Integer] ->[Integer]
partitionGreater pivot [] = []
partitionGreater pivot (x:xs)
    | x > pivot = x: partitionGreater pivot xs
    | otherwise = partitionGreater pivot xs


quickSort :: [Integer] ->[Integer]
quickSort [] = []
quickSort (pivot:xs) = partitionLess pivot xs ++ [pivot] ++ partitionGreater pivot xs