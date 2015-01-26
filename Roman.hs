


romeNotation :: [String]
romeNotation = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
romeAmount :: [Int]
romeAmount   = [1000, 900, 500, 400, 100,   90,   50,  40, 10,    9,   5,    4,   1]
pair :: [(Int, String)]
pair = zip romeAmount romeNotation


subtrahend :: Int -> (Int, String)
subtrahend n = head $ dropWhile (\(a, _) -> a > n) pair

initStep :: Int -> (Int, String)
initStep n = (n, "")
subStep :: (Int, String) -> (Int, String)
subStep (n, s) = ((n - (fst $ subtrahend n)), (concat [s, (snd $ subtrahend n)])) -- (concat s (snd $ subtrahend n)))

tryOneStep :: Int -> (Int, String)
tryOneStep n = subStep $ initStep n

convertImpl :: (Int, String) -> (Int, String)
convertImpl (0, s) = (0, s)
convertImpl (n, s) = convertImpl $ subStep (n, s)
convert :: Int -> String
convert 0 = ""
convert n = snd (convertImpl $ initStep n)

convert2 :: Int -> (Int, String)
convert2 0 = (0, "")
convert2 n = let (i, st) = subtrahend n in
              let (i', st') = convert2(n-i) in (i', st++st')
