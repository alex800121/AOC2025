module Day9 where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Ix (Ix (inRange))
import Data.List (find, sort, tails)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Debug.Trace (traceShow)
import Paths_AOC2025 (getDataDir)

day9a l =
  [ a
  | i@(x0, y0) : xs <- tails l
  , j@(x1, y1) <- xs
  , let a = (abs (x0 - x1) + 1) * (abs (y0 - y1) + 1)
  ]

{-
Calc a list of available y ranges at every line sweep.

A new point added can inherit the y-range it belongs to,
and this y-range can only be reduced.

An existing point should check at every line sweep for the points within range,
and add the resulting areas to the candidates.

1,3
1,5
3,1
3,3
5,5
5,6
5,2
5,4
7,1
7,2
6,4
6,6

  0        1
  1234567890123
 +-------------
1|  c***i
2|  **g*j
3|a*d**
4|****hk
5|b***e*
6|    fl

-}

process acc ys m [] = acc
process acc ys m ((x0, ra) : xs) = process acc' ys' m'' xs
  where
    ys' = foldl' calcRange ys ra
    newRange y = filter (`inRange` y) ys'
    m' = Map.fromList [((x0, y), r) | (y0, y1) <- ra, y <- [y0, y1], r <- newRange y] <> m
    updateRange (_, y) (oy0, oy1) (ny0, ny1)
      | y1 > y0 && inRange r y = Just r
      | otherwise = Nothing
      where
        y0 = max ny0 oy0
        y1 = min ny1 oy1
        r = (y0, y1)
    m'' = Map.mapMaybeWithKey (\k e -> listToMaybe (mapMaybe (updateRange k e) ys')) m'
    acc' = maximum (acc : newAreas)
    newAreas =
      [ a
      | ((x, y), r) <- Map.toList m
      , (y0, y1) <- ra
      , y' <- [y0, y1]
      , inRange r y'
      , let a = (abs (x - x0) + 1) * (abs (y - y') + 1)
      ]
    calcRange [] (y0, y1) = [(y0, y1)]
    calcRange ((x0, x1) : xs) (y0, y1)
      | y1 < x0 = (y0, y1) : (x0, x1) : xs
      | y1 == x0 = calcRange xs (y0, x1)
      | y0 == x0 && y1 == x1 = xs
      | y0 == x0 = (y1, x1) : xs
      | y0 > x0 && y1 < x1 = (x0, y0) : (y1, x1) : xs
      | y1 == x1 = (x0, y0) : xs
      | y0 == x1 = calcRange xs (x0, y1)
      | y0 > x1 = (x0, x1) : calcRange xs (y0, y1)

f (a : b : xs) = (a, b) : f xs
f _ = []
preprocess l = IM.toList $ IM.map (f . IS.toList) m0
  where
    m0 = foldr (\(a, b) -> IM.insertWith (<>) a (IS.singleton b)) IM.empty l

day9 :: IO (String, String)
day9 = do
  input <- map ((\[x, y] -> (read @Int x, read @Int y)) . splitOn ",") . lines <$> (readFile . (++ "/input/input9.txt") =<< getDataDir)
  -- input <- map ((\[x, y] -> (read @Int x, read @Int y)) . splitOn ",") . lines <$> (readFile . (++ "/input/test9.txt") =<< getDataDir)
  let !finalAnsa =
        show $ maximum $ day9a input
  let !finalAnsb =
        show
          . process 0 [] Map.empty
          $ preprocess input
  pure (finalAnsa, finalAnsb)
