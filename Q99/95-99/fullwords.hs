import Data.Char
import Data.List
 
fullWords :: Integer -> String
fullWords n = concat $ intersperse "-" [digits!!digitToInt d | d <- show n]
  where digits = ["zero", "one", "two", "three", "four",
                  "five", "six", "seven", "eight", "nine"]
