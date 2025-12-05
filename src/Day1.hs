module Day1 where

import Data.List
import Paths_AOC2025 (getDataDir)
import Text.Read (readMaybe)

f (acc, n) x
  | 'R' : x <- x
  , Just x <- readMaybe @Int x
  , (d, m) <- (x + n) `divMod` 100 =
      (acc + d, m)
  | 'L' : x <- x
  , Just x <- readMaybe @Int x
  , (d, m) <- (((100 - n) `mod` 100) + x) `divMod` 100 =
      (acc + d, (100 - m) `mod` 100)

day1 :: IO (String, String)
day1 = do
  input <- lines <$> (readFile . (++ "/input/input1.txt") =<< getDataDir)
  -- input <- lines <$> (readFile . (++ "/input/test1.txt") =<< getDataDir)
  let !finalAnsa =
        show
          . length
          . filter (== 0)
          $ scanl' (\acc x -> (acc + (case x of 'R' : n -> read n; 'L' : n -> -(read n))) `mod` 100) 50 input
  let !finalAnsb =
        show
          . fst
          $ foldl' f (0, 50) input
  pure (finalAnsa, finalAnsb)
