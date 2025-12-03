module Day2 where

import Data.Containers.ListUtils (nubOrd)
import Data.Function (fix)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Paths_AOC2025 (getDataDir)

invalid (x, y) = ans
 where
  x10 = log10 x + 1
  y10 = log10 y + 1
  ans =
    nubOrd
      [ z
      | r <- [-((-x10) `div` 2) .. (y10 `div` 2)]
      , a <- [10 ^ (r - 1) .. (10 ^ r) - 1]
      , let z = a * (10 ^ r) + a
      , z >= x && z <= y
      ]
invalid' (x, y) = ans
 where
  x10 = log10 x + 1
  y10 = log10 y + 1
  ans =
    nubOrd
      [ z
      | r <- [1 .. y10 `div` 2]
      , xy10 <- [x10 .. y10]
      , let (d, m) = xy10 `divMod` r
      , m == 0
      , d > 1
      , a <- [10 ^ (r - 1) .. (10 ^ r) - 1]
      , let z = a * sum (map ((10 ^) . (r *)) [0 .. d - 1])
      , z >= x && z <= y
      ]

log10 = fix (\f acc n -> let n' = n `div` 10 in if n' == 0 then acc else f (acc + 1) n') 0

-- >>> log10 22
-- >>> (-5) `div` (2)
-- 1
-- -3

day2 :: IO ()
day2 = do
  input <- map ((\[x, y] -> (read @Int x, read @Int y)) . splitOn "-") . splitOn "," . init <$> (readFile . (++ "/input/input2.txt") =<< getDataDir)
  -- input <- map ((\[x, y] -> (read @Int x, read @Int y)) . splitOn "-") . splitOn "," . init <$> (readFile . (++ "/input/test2.txt") =<< getDataDir)
  let maxN = maximum $ map (uncurry max) input
  putStrLn
    . ("day2a: " ++)
    . show
    . sum
    $ concatMap invalid input
  putStrLn
    . ("day2b: " ++)
    . show
    . sum
    $ concatMap invalid' input
