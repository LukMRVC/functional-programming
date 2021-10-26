doubleMe x = x * x

plus::Int -> Int -> Int
plus x y = x + y


pythagoras::Double -> Double -> Double
pythagoras a b = sqrt (a * a + b * b)

max :: Int -> Int -> Int
max x y | x >= y = x
        | otherwise = y


factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial 2 = 2
factorial n = n * factorial (n -1)