module Day6 where

import Data.List
import Paths_AOC2025 (getDataDir)
import Data.List.Split (splitWhen)
import Data.Char (isDigit)

day6a s = foldl1' x xs
 where
  Just (ys, y) = unsnoc s
  x = case y of
    "+" -> (+)
    "*" -> (*)
  xs = map read ys

day6b :: [String] -> Int
day6b (s : ss) = foldl1' x xs
  where
    Just (ys, y) = unsnoc s
    x = case y of
      '+' -> (+)
      '*' -> (*)
    xs = map (read . filter isDigit) (ys : ss)

day6 :: IO (String, String)
day6 = do
  input <- lines <$> (readFile . (++ "/input/input6.txt") =<< getDataDir)
  -- input <- lines <$> (readFile . (++ "/input/test6.txt") =<< getDataDir)
  let !finalAnsa =
        show
          . sum
          . map day6a
          . transpose
          $ map words input
  let !finalAnsb =
        show  
          . sum
          . map day6b
          . splitWhen (all (== ' '))
          $ transpose input
  pure (finalAnsa, finalAnsb)
