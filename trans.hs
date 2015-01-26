import Data.Char (ord, chr, isLower)

char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char i = chr (ord 'a' + i)

shift :: Int -> Char -> Char
shift n c | isLower c = int2char $ ((char2int c) + n) `mod` 26
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

try_decode :: String -> [String]
try_decode xs = [ encode n ys | ys <-[xs], n<-[1..25] ]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0,   6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

lowers :: String -> Int
lowers xs = length [ x | x<-xs, isLower x]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Char -> [Char] -> Int
count c xs = length [ x | x <-xs, x == c ]

freqs :: String -> [Float]
freqs xs | lowers xs == 0 = [ 100.0/3.0 | _ <- [1..26] ]
         | otherwise = [ percent (count x xs) n | x <-['a'..'z'] ]
          where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ (o-e)^2 | (o,e) <- zip os es]

min_first :: [(Float, String)] -> (Float, String)
min_first (x:[]) = x
min_first (x:y:ys) = if fst x < fst y then min_first (x:ys) else min_first (y:ys)

crack :: String -> String
crack xs = let posiable =  try_decode xs in
            let chisqr_s = [ (chisqr (freqs p) table, p) | p <- posiable ] in
              let min_chisqr = min_first chisqr_s in
                snd min_chisqr

