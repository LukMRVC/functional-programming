type Result = [String]

pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

draw :: [(Char, Int)] -> Result
draw moves = let
    pos = positions (0, 0) moves
    rows = map (fst) pos
    columns = map (snd) pos
    (minRow, maxRow) = (minimum rows, maximum rows)
    (minCol, maxCol) = (minimum columns, maximum columns)
    in [[if elem (row, column) pos then 'X' else ' ' | column <- [minCol..maxCol]  ]| row <- [minRow..maxRow]]


positions :: (Int, Int) -> [(Char, Int)] -> [(Int, Int)]
positions _ [] = []
positions (row, column) (m:moves) = let
    pos = oneLine row column m
    in pos ++ positions (last pos) moves

oneLine row column direction length
    | direction == 'l' = [(row, column - offset) | offset <- [1..length]] 
    | direction == 'p' = [(row, column + offset) | offset <- [1..length]] 
    | direction == 'u' = [(row + offset, column) | offset <- [1..length]] 
    | direction == 'd' = [(row - offset, column) | offset <- [1..length]] 


