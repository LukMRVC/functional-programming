type Result = [String]

pp :: Result -> IO ()
pp x = putStr (concatMap (++"\n") x)

data Point = Point Int Int
    deriving (Show, Eq)

data Shape = Circle Point Int
           | Rectangle {topLeft:: Point, bottomRight::Point}
          deriving (Show)

drawCircle :: [Point] -> Result
drawCircle pos = let
    rows = map (\ (Point x y) -> x) pos
    columns = map (\ (Point x y) -> y) pos
    (minRow, maxRow) = (minimum rows, maximum rows)
    (minCol, maxCol) = (minimum columns, maximum columns)
    in [[if Point row column `elem` pos then 'X' else ' ' | column <- [minCol..maxCol]  ]| row <- [minRow..maxRow]]

recalcD :: Int -> Int -> Int -> Int -> Int
recalcD x y d 0 = d + 4 * (x - y) + 10
recalcD x y d 1 = d + 4 * x + 6
recalcD _ _ _ _ = 0


circlePoints :: Point -> Point -> [Point]
circlePoints (Point cx cy) (Point x y) = [Point (cx `xop` x0) (cy `yop` y0) | (x0, y0) <- [(x, y), (y, x)], xop <- [(+), (-)], yop <- [(+), (-)]]

sampleRect = Rectangle {topLeft = Point 15 5, bottomRight = Point 35 12 }
sampleCircle = Circle (Point 8 4) 5

-- Bresenham circle algorithm
generateShape :: Shape -> [Point]
generateShape (Rectangle (Point tx ty) (Point bx by)) = [Point x y | x <- [tx..bx] , y <- [ty, by]] ++ [Point x y | y <- [ty..by], x <- [tx, bx]]
generateShape (Circle center radius) = circlePoints center (Point 0 radius) ++ drawUntil 0 radius (3 - 2 * radius) where
  drawUntil ::Int -> Int -> Int -> [Point]
  drawUntil x y d  | y >= x = circlePoints center (Point (x + 1) (if d > 0 then y - 1 else y)) ++ if d > 0
    then drawUntil (x + 1) (y - 1) (recalcD x y d 0)
    else drawUntil (x + 1) y (recalcD x y d 1)
                          | otherwise = circlePoints center (Point (x + 1) (if d > 0 then y - 1 else y))



view :: (Int, Int) -> [Shape] -> Result
view (w, h) shapes = let 
  shapePointData = concatMap generateShape shapes
  output = [[if Point x y `elem` shapePointData then '#' else '.' | x <- [0..w]] | y <- [0..h]]
  in output