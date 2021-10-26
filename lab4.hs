import Data.Char (toUpper)


length'::Foldable t => t a -> Int
length' x = foldl (\x y -> x + 1) 0 x

zipThem :: [a] -> [b] -> [(a, b)]
zipThem (x:xs) (y:ys) = (x, y) : zipThem xs ys
zipThem _ _ = []

dotProduct :: [a] -> [b] -> [(a, b)]
dotProduct [] _ = []
dotProduct (x:xs) ys = tmp x ys ++ dotProduct xs ys where
    tmp x [] = []
    tmp x (y:ys) = (x, y) : tmp x ys

allToUpper :: String -> String
allToUpper (x:xs) = toUpper x : allToUpper xs


quicksort :: Ord(a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<=x) xs) ++ x: quicksort (filter (>= x) xs)

oddList :: Int -> Int -> [Int]
oddList x y = [x | x <- [x..y], odd x]


countThem :: String -> [(Char, Int)]
countThem [] = []
countThem (x:xs) = (x, length (filter (==x) (x:xs))) : countThem (filter (/=x) xs)


isPrime::Int -> Bool
isPrime n  = prime (truncate (sqrt (fromIntegral n))) where
        prime :: Int -> Bool
        prime 1 = True
        prime c | n `mod` c == 0 = False
                  | otherwise = prime (c - 1)

goldbach :: Int -> [(Int, Int)]
goldbach x = solveGolbach [y | y <- [1..x], isPrime y] x where
    solveGolbach (y:z:ys) x = if z + y == x then (y, z): solveGolbach (y:ys) x else solveGolbach (z:ys) x
    solveGolbach [y, z] x   | y + z == x = [(y, z)]
                            | otherwise = []