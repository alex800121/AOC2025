module Day11 where

import Control.Arrow
import Data.Containers.ListUtils (nubOrd)
import Data.Graph
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import Paths_AOC2025 (getDataDir)
import Data.Array.IArray qualified as IA

readInput input = (im, sk, n)
  where
    l = map (map words . splitOn ": ") $ lines input
    (im, sk, n) = foldl' f (IM.empty, M.empty, 0) l
    f (im, sk, n) [x : _, xs] = foldl' (g x) (im, sk, n) xs
    g x (im, sk, n) y = (IM.insertWith (<>) ny [nx] im, M.fromList xy <> sk, maximum [n, nx, ny])
      where
        nx = fromMaybe (n + 1) (sk M.!? x)
        ny = fromMaybe (max n nx + 1) (sk M.!? y)
        xy = [(x, nx), (y, ny)]

allPaths acc g start fin
  | start == fin = (acc, 1)
  | Just x <- acc IM.!? fin = (acc, x)
  | Just ys <- g IA.!? fin = let (acc', n) = foldl' (\(a, n) x -> second (+ n) $ allPaths a g start x) (acc, 0) ys in (IM.insert fin n acc', n)

solveB g (x : y : xs) = snd (allPaths IM.empty g x y) * solveB g (y : xs)
solveB g _ = 1
day11 :: IO (String, String)
day11 = do
  (im, sk, n) <- readInput <$> (readFile . (++ "/input/input11.txt") =<< getDataDir)
  let g' = buildG (0, n) $ [(f, t) | (f, ts) <- IM.toList im, t <- ts]
      start = sk M.! "you"
      fin = sk M.! "out"
  let !finalAnsa =
        show . snd $ allPaths IM.empty g' start fin
  let !finalAnsb =
        show $ sum [solveB g' pathI | path <- [["svr", "dac", "fft", "out"], ["svr", "fft", "dac", "out"]], let pathI = map (sk M.!) path]
  pure (finalAnsa, finalAnsb)
