import Data.List


type Result = [String]

pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

data Point = Point Int Int
    deriving (Show, Eq)

data Shape = Circle Point Int
           | Rectangle {topLeft:: Point, bottomRight::Point}


drawCircle :: [Point] -> Result
drawCircle pos = let
    rows = map (\ (Point x y) -> x) pos
    columns = map (\ (Point x y) -> y) pos
    (minRow, maxRow) = (minimum rows, maximum rows)
    (minCol, maxCol) = (minimum columns, maximum columns)
    in [[if elem (Point row column) pos then 'X' else ' ' | column <- [minCol..maxCol]  ]| row <- [minRow..maxRow]]

generateCirclePoints :: Point -> Int -> [Point]
generateCirclePoints (Point x0 y0) radius = Point x0 (y0 + radius) : Point x0 (y0 - radius) : Point (x0 + radius) y0 : Point (x0 - radius) y0 : points
    where
      -- Creates the (x, y) octet offsets, then maps them to absolute points in all octets.
      points = concatMap generatePoints $ unfoldr step initialValues
      generatePoints (x, y)
        = [Point (xop x0 x') (yop y0 y') | (x', y') <- [(x, y), (y, x)], xop <- [(+), (-)], yop <- [(+), (-)]]
 
      -- The initial values for the loop
      initialValues = (1 - radius, 1, (-2) * radius, 0, radius)
 
      -- One step of the loop. The loop itself stops at Nothing.
      step (f, ddf_x, ddf_y, x, y) | x >= y = Nothing
                                   | otherwise = Just ((x', y'), (f', ddf_x', ddf_y', x', y'))
                                     where
                                       (f', ddf_y', y') | f >= 0 = (f + ddf_y' + ddf_x', ddf_y + 2, y - 1)
                                                        | otherwise = (f + ddf_x, ddf_y, y)
                                       ddf_x' = ddf_x + 2
                                       x' = x + 1