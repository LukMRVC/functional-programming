import Data.Char (ord, chr)

type Field = [String]

printField :: Field -> IO ()
printField x = putStr (concat (map (++"\n") x))

elemAt :: Int -> [a] -> a
elemAt x y = findAt 0 x y where
    findAt :: Int -> Int -> [a] -> a
    findAt _ _ [a] = a
    findAt at x (y:ys) = if at == x then y else findAt (at + 1) x ys


intoChar :: Int -> Char
intoChar c = chr (c + 48) 


sampleInput = ["       ",
               " *   **",
               "    *  ",
               " * *   ",
               "      *",
               "***    ",
               "* *    ",
               "***   *"]



sweepField :: Field -> Field
sweepField f = sweepLine [0..length f - 1] f where
    sweepLine :: [Int] -> Field -> Field
    sweepLine _ [] = []
    sweepLine (curIdx:next) (s:restOfField) = checkNeighbours 0 curIdx s: sweepLine next restOfField where
        checkNeighbours :: Int -> Int -> String -> String
        checkNeighbours _ _ "" = ""
        checkNeighbours charIdx lineIdx (c:lineStr) = 
            let
                minesAbove = if (lineIdx - 1) < 0 then 0 else length $ filter (=='*') $ take (length $ filter (>=0) [charIdx-1..charIdx + 1]) $ drop (charIdx - 1) $ elemAt (lineIdx - 1) f
                minesBySide = length $ filter (=='*') $ take (length $ filter (>=0) [charIdx-1..charIdx+1]) $ drop (charIdx - 1) $ elemAt lineIdx f
                minesBelow = if (lineIdx + 1) >= length f then 0 else length $ filter (=='*') $ take (length $ filter (>=0) [charIdx-1..charIdx + 1]) $ drop (charIdx - 1) $ elemAt (lineIdx + 1) f
                result = if c == '*' then '*':checkNeighbours (charIdx + 1) lineIdx lineStr 
                else intoChar (minesBySide + minesAbove + minesBelow): checkNeighbours (charIdx + 1) lineIdx lineStr
            in result

