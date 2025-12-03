module Day3 where

import Data.List
import Paths_AOC2025 (getDataDir)

day3 :: IO ()
day3 = do
  input <- lines <$> (readFile . (++ "/input/input3.txt") =<< getDataDir)
  putStrLn
    . ("day3a: " ++)
    . show
    . sum
    $ map (read @Int . f 2) input
  putStrLn
    . ("day3b: " ++)
    . show
    . sum
    $ map (read @Int . f 12) input

f n input = foldr g xs ys
  where
    (ys, xs) = splitAt (length input - n) input
    g _ [] = []
    g y (z : zs)
      | z > y = z : zs
      | otherwise = y : g z zs
