module Day2 where

import Data.Containers.ListUtils (nubOrd)
import Data.Function (fix)
import Data.List.Split (splitOn)
import Paths_AOC2025 (getDataDir)
import Prelude hiding (log)

validA xs (x, y) =
  nubOrd
    [ zs
    | n <- xs,
      n > 1,
      xy <- [x10 + 1, y10 + 1],
      let (d, m) = xy `divMod` n,
      m == 0 && d > 0,
      z <- [10 ^ (d - 1) .. (10 ^ d) - 1],
      let zs = z * sum (take n (iterate (* (10 ^ d)) 1)),
      zs >= x && zs <= y
    ]
  where
    x10 = log 10 x
    y10 = log 10 y

log = fix (\f c y -> let y' = y `div` c in if y' > 0 then 1 + f c y' else 0)

day2 :: IO (String, String)
day2 = do
  input <-
    map ((\[x, y] -> (read @Int x, read @Int y)) . splitOn "-")
      . splitOn ","
      <$> (readFile . (++ "/input/input2.txt") =<< getDataDir)
  let m = log 10 $ maximum $ map snd input
  let !finalAnsa =
        show
          . sum
          $ concatMap (validA [2]) input
  let !finalAnsb =
        show
          . sum
          $ concatMap (validA [2 .. m]) input
  pure (finalAnsa, finalAnsb)
