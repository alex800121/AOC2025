module Day10 where

import AI.Search.FiniteDomain.Int qualified as FD
import Control.Arrow
import Control.Monad (replicateM)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Bits (Bits (..))
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List (find, findIndex, foldl1', sort, sortBy, subsequences, transpose, unfoldr, unsnoc, (!?))
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Ratio (denominator, numerator)
import Data.Tuple (swap)
import Paths_AOC2025 (getDataDir)

readInput input = ((target, button), requirements)
  where
    x : xs = input
    Just (ys, y) = unsnoc xs
    target = foldr (\x acc -> case x of '#' -> acc * 2 + 1; '.' -> acc * 2; _ -> acc) (0 :: Int) x
    button = map f ys
    f xs = map (read @Int) . splitOn "," . tail $ init xs
    requirements = map (read @Int) . splitOn "," . tail $ init y

gausElim :: (Ord a, Fractional a, Num a) => [[a]] -> [[a]]
gausElim l = filter (any (/= 0)) $ go0 sorted
  where
    sorted = sort' l
    sort' = sortBy (compare `on` length . takeWhile (== 0))
    go0 [] = []
    go0 (x : xs)
      | (z, y : ys) <- span (== 0) x
      , xNormalized <- map (/ y) x =
          xNormalized : go0 (sort' $ map (f (length z) xNormalized) xs)
      | otherwise = x : go0 xs
    f n x xs
      | Just y <- xs !? n, y /= 0 = zipWith (\a b -> b - (a * y)) x xs
      | otherwise = xs

gausJordElim :: (Ord a, Fractional a, Num a) => [[a]] -> [[a]]
gausJordElim l = go0 [] revGaused
  where
    revGaused = reverse $ gausElim l
    go0 acc [] = acc
    go0 acc (x : xs) = go0 (x : acc) (map (f (length (takeWhile (== 0) x)) x) xs)
      where
        f n x xs
          | Just y <- xs !? n, y /= 0 = zipWith (\a b -> b - (a * y)) x xs
          | otherwise = xs

toDec = foldl' (\acc x -> acc * 10 + x) 0
day10b buttons req = go IM.empty req
  where
    len = length req
    allComb = M.assocs $ M.fromListWith min $ build buttons
    build [] = [(replicate len 0, 0)]
    build (x : xs) =
      [ (zipWith (+) i j, n + m)
      | (i, n) <- [(replicate len 0, 0), (toInvBits len (foldl' setBit (0 :: Int) x), 1)]
      , (j, m) <- build xs
      ]
    go acc !r
      | all (== 0) r = (acc, 0 :: Int)
      | Just n <- acc IM.!? toDec r = (acc, n)
      | otherwise = (IM.insert rDec n' acc', n')
      where
        rDec = toDec r
        (acc', !n') = foldl' f (acc, 10000000) allComb
        f (acc, !n) (!a, !m)
          | all even r' && all (>= 0) r' = (acc', n'')
          | otherwise = (acc, n)
          where
            r' = zipWith (-) r a
            r'' = map (`div` 2) r'
            (acc', n') = go acc r''
            n'' = min n (m + 2 * n')

day10a t0 =
  find
    ( (== t0)
        . foldl' xor 0
    )
    . sortBy (compare `on` length)
    . tail
    . subsequences
    . map (foldl' setBit (0 :: Int))

normalize bs r = transpose (map (map fromIntegral . toInvBits l . foldl' setBit (0 :: Int)) bs ++ [map (fromIntegral . negate) r])
  where
    l = length r

toInvBits l = take l . unfoldr (\x -> Just (swap (x `divMod` 2)))

toInt l = map (\x -> numerator x * (d `div` denominator x)) l
  where
    d = foldr (\x acc -> lcm acc (denominator x)) 1 l

calcConstraint ((_, bs), m) = foldl' (\acc (i, b) -> IM.unionWith min acc (IM.fromList (map ((i,) . (m !!)) b))) IM.empty ibs
  where
    ibs = zip [0 ..] bs

day10 :: IO (String, String)
day10 = do
  input <- map (readInput . words) . lines <$> (readFile . (++ "/input/input10.txt") =<< getDataDir)
  -- let simplified = map ((map toInt . gausJordElim . uncurry normalize . first snd) &&& (IM.elems . calcConstraint)) input
  --     fd (l, ms) = do
  --       xs <- replicateM (length (head l) - 1) FD.newVar
  --       mapM_ (FD.int 0 FD.#<=) xs
  --       mapM_ (\(a, b) -> FD.int a FD.#>= b) (zip ms xs)
  --       mapM_ (f xs) l
  --       FD.labeling xs
  --       where
  --         f xs l = FD.sum (concat a) FD.#= FD.sum (FD.int (negate y) : concat b)
  --           where
  --             Just (ys, y) = unsnoc l
  --             f a b = if a > 0 then Left (replicate a b) else Right (replicate (-a) b)
  --             (a, b) = partitionEithers $ zipWith f ys xs
  let !finalAnsa =
        show
          . sum
          $ mapMaybe (fmap length . uncurry day10a . fst) input
  let !finalAnsb =
        show
          . sum
          $ parMap rpar (snd . uncurry day10b . first snd) input
  pure (finalAnsa, finalAnsb)
