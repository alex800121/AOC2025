module Day3 where

import Data.List
import Paths_AOC2025 (getDataDir)

f n input = foldr g xs ys
  where
    (ys, xs) = splitAt (length input - n) input
    g _ [] = []
    g y (z : zs)
      | z > y = z : zs
      | otherwise = y : g z zs

day3 :: IO (String, String)
day3 = do
  input <- lines <$> (readFile . (++ "/input/input3.txt") =<< getDataDir)
  let !finalAnsa =
        show
          . sum
          $ map (read @Int . f 2) input
  let !finalAnsb =
        show
          . sum
          $ map (read @Int . f 12) input
  pure (finalAnsa, finalAnsb)
