module Day4 where

import Data.Array.IArray qualified as IA
import Data.Array.Unboxed (UArray)
import MyLib (drawArray, (+&))
import Paths_AOC2025 (getDataDir)

type Index = (Int, Int)

adjacent = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]
day4a a =
    [ (i, '.')
    | (i, '@') <- IA.assocs a
    , let is = map (i +&) adjacent
    , length (filter ((== Just '@') . (a IA.!?)) is) < 4
    ]

day4b = go 0
  where
    go acc x 
      | null removed = acc
      | otherwise = go (acc + length removed) x'
      where
        removed = day4a x
        x' = x IA.// removed
day4 :: IO ()
day4 = do
  input <- drawArray @UArray . lines <$> (readFile . (++ "/input/input4.txt") =<< getDataDir)
  putStrLn
    . ("day4a: " ++)
    . show
    . length
    $ day4a input
  putStrLn
    . ("day4b: " ++)
    . show
    $ day4b input
