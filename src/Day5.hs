module Day5 where

import Data.Ix (Ix (..))
import Data.List (sort)
import Data.List.Split (splitOn)
import Paths_AOC2025 (getDataDir)

fresh r i = any (`inRange` i) r
day5 :: IO (String, String)
day5 = do
  (r, i) <-
    (\[a, b] -> (map (\[x, y] -> (x, y)) a, concat b))
      . map (map (map (read @Int) . splitOn "-") . lines)
      . splitOn "\n\n"
      <$> (readFile . (++ "/input/input5.txt") =<< getDataDir)
  let !finalAnsa =
        show
          . length
          $ filter (fresh r) i
  let !finalAnsb =
        show . f (0, (0, -1)) $ sort r
  pure (finalAnsa, finalAnsb)

f (acc, (m, n)) [] = acc + n - m + 1
f (acc, (m, n)) ((a, b) : xs)
  | a > n = f (acc + n - m + 1, (a, b)) xs
  | b > n = f (acc, (m, b)) xs
  | otherwise = f (acc, (m, n)) xs
