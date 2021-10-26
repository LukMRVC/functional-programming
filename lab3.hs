length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

isElement :: Eq a => a -> [a] -> Bool
isElement _ [] = False
isElement x (y:ys)  | x == y = True
                    | otherwise = isElement x ys

getInit :: [a] -> [a]
getInit [a] = []
getInit (x:xs) = x : getTail xs


getTail :: [a] -> [a]
getTail (x:xs) = xs


combine :: [a] -> [a] -> [a]
combine [x] y = x:y
combine (x:xs) y = x : combine xs y

max' :: [Int] -> Int
max' (x:xs) = maximum xs x where
    maximum :: [Int] -> Int -> Int
    maximum [] a = a
    maximum (x:xs) a = if x > a then maximum xs x else maximum xs a

reverse' :: [a] -> [a]
reverse' [a] = [a]
reverse' (x:xs) = combine (reverse' xs) [x]

reverse'' :: [a] -> [a]
reverse'' xs = tmpRev xs [] where
    tmpRev :: [a] -> [a] -> [a]
    tmpRev [] ys = ys
    tmpRev (x:xs) ys = tmpRev xs (x:ys)


take' :: Int -> [a] -> [a]
take' 0 xs = []
take' n [] = []
take' n (x:xs) = x: take' (n-1) xs


-- drop' :: Int -> [a] -> [a]

divisors :: Int -> [Int]
divisors n = tmp n where
    tmp 1 = [1]
    tmp c   | n `mod` c == 0 = c : tmp (c-1)
            | otherwise = tmp (c - 1)

divisors' :: Int -> [Int]
divisors' n = [x | x <- reverse' [1..n], n `mod` x == 0]
