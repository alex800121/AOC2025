module Day10 where

import Control.Arrow
import Control.Monad.ST.Strict (runST)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Bits (Bits (..))
import Data.Function (on)
import Data.IntMap.Strict qualified as IM
import Data.List (nubBy, sort, sortBy, subsequences, unsnoc)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as Map
import Data.Vector.Strict qualified as V
import Data.Vector.Strict.Mutable qualified as MV
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as MUV
import Data.Word (Word16)
import Debug.Trace
import Paths_AOC2025 (getDataDir)

readInput input = (allComb button, (target, requirements))
  where
    x : xs = input
    Just (ys, y) = unsnoc xs
    target = foldr (\x acc -> case x of '#' -> acc * 2 + 1; '.' -> acc * 2; _ -> acc) (0 :: Int) x
    button = map f ys
    f xs = take len . flip (snd . foldl' g (0, id) . map (read @Int) . splitOn "," . tail) (repeat 0) $ init xs
    len = UV.length requirements
    requirements = UV.fromList . map (read @Int) . splitOn "," . tail $ init y
    g (n, f) m = (m + 1, f . (replicate (m - n) 0 <>) . ((1 :: Int) :))

toInt = UV.foldr (\x acc -> if odd x then acc * 2 + 1 else acc * 2) 0

allComb x = runST $ do
  output <- MV.replicate (2 ^ len) []
  mapM_
    (\(k, n) -> MV.modify output ((k, n) :) (toInt k))
    m
  mapM_
    (MV.modify output (nubBy ((==) `on` fst) . sort))
    [0 .. len - 1]
  V.freeze output
  where
    len = length (head x)
    m =
      map (foldr (UV.zipWith (+) . UV.fromList) (UV.replicate len 0) &&& length) $
        subsequences x

day10a comb target = minimum . map snd $ (comb V.! target)

day10b acc comb target
  | UV.all (== 0) target = (acc, 0)
  | Just n <- acc Map.!? target = (acc, n)
  | otherwise = (Map.insert target n acc', n)
  where
    t0 = toInt target
    (acc', n) =
      foldl'
        f
        (acc, 10000000)
        [ (target', i)
        | (v, i) <- comb V.! t0
        , let target' = UV.zipWith subtract v target
        , UV.all (liftA2 (&&) (>= 0) even) target'
        ]
    f (acc0, n) (target, i) = (acc', min n (2 * n' + i))
      where
        (acc', n') = day10b acc0 comb (UV.map (`div` 2) target)

day10 :: IO (String, String)
day10 = do
  input <- map (readInput . words) . lines <$> (readFile . (++ "/input/input10.txt") =<< getDataDir)
  let !finalAnsa =
        show
          . sum
          $ parMap rpar (uncurry day10a . second fst) input
  let !finalAnsb =
        show
          . sum
          $ parMap rpar (snd . uncurry (day10b Map.empty) . second snd) input
  pure (finalAnsa, finalAnsb)
