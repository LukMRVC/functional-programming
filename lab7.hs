type Pic = [String]

pp :: Pic -> IO ()
pp x = putStr (concat (map (++"\n") x))

pic :: Pic
pic = [ "....#....",
        "...###...",
        "..#.#.#..",
        ".#..#..#.",
        "....#....",
        "....#....",
        "....#####"]


flipH :: Pic -> Pic
flipH = reverse

flipV :: Pic -> Pic
flipV = map reverse

above :: Pic -> Pic -> Pic
above = (++)

sideBySide :: Pic -> Pic -> Pic
sideBySide = zipWith (++)

lineToRow :: String -> Pic
lineToRow [] = []
lineToRow (x:xs) = [x]: lineToRow xs

rotateR :: Pic -> Pic
rotateR [x] = lineToRow x
rotateR (x:xs) = rotateR xs `sideBySide` lineToRow x

rotateL :: Pic -> Pic
rotateL x = flipV (flipH (rotateR x))