{-# LANGUAGE MultiWayIf #-}

module Day8 where

import Control.Arrow
import Control.Monad.ST.Strict (ST, runST)
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Vector.Algorithms.Radix
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as MUV
import Data.Word (Word32, Word64)
import Optics
import Paths_AOC2025 (getDataDir)

distSqr :: (Int, Int, Int) -> (Int, Int, Int) -> Int
distSqr (a, b, c) (d, e, f) = (a - d) ^ 2 + (b - e) ^ 2 + (c - f) ^ 2

readInput :: String -> (UV.Vector (Int, (Int, Int)), UV.Vector Int)
readInput input = (ils, v)
  where
    ls = map ((\[a, b, c] -> (a, b, c)) . map (read @Int) . splitOn ",") $ lines input
    v = UV.fromList $ map (view _1) ls
    ils =
      UV.modify (sortBy (passes (0 :: Int)) (size (0 :: Int)) (\i e -> radix i (fst e)))
        . UV.fromList
        . f
        $ zip [0 ..] ls
    f ((a, b) : xs) = map (\(c, d) -> (distSqr b d, (a, c))) xs <> f xs
    f _ = []

find :: MUV.STVector s (Int, Word32) -> Int -> ST s (Int, Word32)
find mother !n = do
  (m, i) <- MUV.read mother n
  if n == m
    then pure (m, i)
    else do
      (!x, !j) <- find mother m
      MUV.modify mother (first (const x)) n
      pure (x, j)

union :: Int -> MUV.STVector s (Int, Word32) -> Int -> Int -> ST s Int
union !len mother a b = do
  (ma, ia) <- find mother a
  (mb, ib) <- find mother b
  if
    | ma == mb -> pure len
    | ia < ib -> do
        MUV.write mother ma (mb, 0)
        MUV.modify mother (second (+ ia)) mb
        pure (len - 1)
    | otherwise -> do
        MUV.write mother mb (ma, 0)
        MUV.modify mother (second (+ ib)) ma
        pure (len - 1)

day8a :: Int -> UV.Vector (Int, (Int, Int)) -> ST s Word32
day8a len xs = do
  mother <- MUV.generate len (,1)
  UV.mapM_ (uncurry (union maxBound mother) . snd) xs
  sortBy (passes (0 :: Word32)) (size (0 :: Word32)) (\i e -> radix i (snd e)) mother
  product . map snd <$> mapM (MUV.read mother . (len -)) [1, 2, 3]

day8b v xs = do
  mother <- MUV.generate len (,1)
  let f n !len = do
        let (_, (!a, !b)) = xs UV.! n
        !len' <- union len mother a b
        if len' == 1
          then pure (v UV.! a * v UV.! b)
          else f (n + 1) len'
  f 0 len
  where
    len = UV.length v

day8 :: IO (String, String)
day8 = do
  (ls, v) <-
    readInput
      <$> (readFile . (++ "/input/input8.txt") =<< getDataDir)
  let !finalAnsa =
        show $ runST $ day8a (UV.length v) $ UV.take 1000 ls
  let !finalAnsb =
        show $ runST $ day8b v ls
  pure (finalAnsa, finalAnsb)
