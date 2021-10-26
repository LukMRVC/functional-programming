factorial::Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial 2 = 2
factorial n = n * factorial (n -1)

fib::Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib2 n = step 1 1 n where
    step a b 0 = b
    step a b 1 = b
    step a b n = step b (a+b) (n-1)


leapYear::Int -> Bool
leapYear year = mod year 400 == 0 || (mod year 4 == 0 && mod year 100 /= 100)

leapYear2 y | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | otherwise = y `mod` 4 == 0

-- max2::Int -> Int -> Int
-- max2 a b = max a b

max2::Int -> Int -> Int
max2 x y = if x < y then y else x

max3::Int -> Int -> Int -> Int
max3 a b c = max2 (max2 a b) c

gcd' ::Int -> Int -> Int
gcd' a b | a == b = b
         | a < b = gcd' (b - a) a
         | a > b = gcd' (a - b) b

isPrime::Int -> Bool
isPrime n  = prime (truncate (sqrt (fromIntegral n))) where
        prime :: Int -> Bool
        prime 1 = True
        prime c | n `mod` c == 0 = False
                  | otherwise = prime (c - 1)


