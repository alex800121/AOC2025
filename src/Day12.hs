module Day12 where

import Data.Char (digitToInt)
import Data.List (partition, unsnoc)
import Data.List.Split (splitOn)
import Paths_AOC2025 (getDataDir)

readInput input = (pieces, boards)
  where
    Just (xs, x) = unsnoc $ splitOn "\n\n" input
    pieces = map (tail . lines) xs
    boards = map f $ lines x
    f l = ((read @Int [a, b], read @Int [c, d]), map (read @Int) (words xs))
      where
        a : b : _ : c : d : _ : _ : xs = l

solveA pieces ((x, y), req) = (x `div` 3) * (y `div` 3) - sum req
day12 :: IO (String, String)
day12 = do
  (pieces, boards) <- readInput <$> (readFile . (++ "/input/input12.txt") =<< getDataDir)
  let !finalAnsa =
        show $ length $ filter (>= 0) $ map (solveA pieces) boards
  let !finalAnsb = "Merry Christmas!!!"
  pure (finalAnsa, finalAnsb)
