import System.Random
import Control.Monad (replicateM)
import Data.List (nub)

randomSelect :: [a] -> Int -> IO [a]
randomSelect [] _ = return []
randomSelect l n
  | n < 0 = error "n must be greater than 0"
  | otherwise = do pos <- replicateM n $ getStdRandom $ randomR (0, (length l)-1)
                   return [l!!p | p <- pos]

diffSelect :: Int -> Int -> IO [Int]
diffSelect n maxN = do
  get <- newStdGen
  return . take n . nub $ randomRs (0, maxN) get

-- random permutation
randomPermutation :: [a] -> IO [a]
randomPermutation xs = do
  ps <- diffSelect (length xs - 1) (length xs - 1)
  return [ xs !! p | p <- ps]

