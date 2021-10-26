decode :: String -> [(Char, String)] -> String
decode "" voc = ""
decode (s:str) voc = replace voc s ++ decode str voc where
    replace :: [(Char, String)] -> Char -> String
    replace [] s = [s]
    replace ((c, x):tr) s = if c == s then decode x voc else replace tr s