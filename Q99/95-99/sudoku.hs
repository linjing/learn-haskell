import System.Environment
import Control.Monad
import Data.List
import Data.Array.IO

-- Prelude Data.Array.IO> :t (newArray (1, 81) 0)
-- (newArray (1, 81) 0)
--   :: (Num e, Num i, Ix i, MArray a e m) => m (a i e)
-- Prelude Data.Array.IO> (newArray (1, 81) 0) :: IO (IOArray Int Int)

type SudokuBoard = IOArray Int Int

getRowNums :: SudokuBoard -> Int -> IO [Int]
getRowNums a y = sequence [readBroad a (x,y) | x <-[1..9] ]
getColNums :: SudokuBoard -> Int -> IO [Int]
getColNums a x = sequence [readBroad a (x,y) | y <-[1..9] ]
getBoxNums a (x,y) = sequence [readBroad a (c+i,r+j) | i<-[0..2], j<-[0..2]]
  where r = (quot (y-1) 3) * 3 + 1
        c = (quot (x-1) 3) * 3 + 1

validNums :: SudokuBoard -> (Int,Int) -> IO [Int]
validNums a (x,y) = do
  r <- getRowNums a y
  c <- getColNums a x
  b <- getBoxNums a (x,y)
  return $ [1..9] \\ (r `union` c `union` b)

findSudoku :: SudokuBoard -> (Int,Int) -> IO (Maybe SudokuBoard)
findSudoku a (10,y) = findSudoku a (1,y+1)
findSudoku a (_, 10) = printSudokuBroad a >> return (Just a)
findSudoku a (x,y) = do
  v <- readBroad a (x,y)
  case v of
    0 -> validNums a (x,y) >>= findSudokuHelper a (x,y)
    _ -> findSudoku a (x+1,y)

findSudokuHelper a (x,y) [] = return Nothing
findSudokuHelper a (x,y) (v:vs) = 
  do writeBroad a (x,y) v
     r <- findSudoku a (x+1,y)
     writeBroad a (x,y) 0
     findSudokuHelper a (x,y) vs

main = do 
  [f] <- getArgs
  a <- (newArray (1,81) 0 :: IO SudokuBoard)
  readFile f >>= readSudokuBroad a
  putStrLn "Original:"
  printSudokuBroad a
  putStrLn "Solutions:"
  findSudoku a (1,1)


readSudokuBroad :: SudokuBoard -> String -> IO ()
readSudokuBroad a xs = sequence_ $ do (i, ys) <- zip [1..9] (lines xs)
                                      (j, n) <- zip [1..9] (words ys)
                                      return $ writeBroad a (j,i) (read n)

printSudokuBroad :: SudokuBoard -> IO ()
printSudokuBroad a = let printLine a y = 
                          mapM (\x -> readBroad a (x,y)) [1..9] >>= mapM_ (putStr . show)
                     in do
                                          putStrLn "-----------"
                                          mapM_ (\y -> putStr "|" >> printLine a y >> putStrLn "|") [1..9]
                                          putStrLn "-----------"

writeBroad :: SudokuBoard -> (Int,Int) -> Int -> IO ()
writeBroad a (x,y) e = writeArray a (x+9*(y-1)) e

readBroad :: SudokuBoard -> (Int,Int) -> IO Int
readBroad a (x,y) = readArray a (x+9*(y-1))
