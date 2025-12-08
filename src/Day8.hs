module Day8 where

import Data.Bifunctor (Bifunctor (first))
import Data.Function (on)
import Data.IntMap.Strict qualified as IM
import Data.List (sort, sortBy, tails)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as Map
import Data.Ord
import Paths_AOC2025 (getDataDir)

distSqr (a, b, c) (d, e, f) = (a - d) ^ 2 + (b - e) ^ 2 + (c - f) ^ 2

connections l =
  sortBy
    (compare `on` uncurry distSqr)
    [ (a, b)
    | (a : xs) <- tails l
    , b <- xs
    ]

connect l = go 0 0 0 Map.empty IM.empty conn
  where
    conn = connections l
    go x y fresh acc im con n
      | Map.size acc == length l && IM.size im == 1 = (undefined, x * y)
      | n == 0 || null l = first (const im) $ go x y fresh acc im con (n - 1)
      | Nothing <- ma
      , Nothing <- mb =
          go x y (fresh + 1) (Map.insert a fresh $ Map.insert b fresh acc) (IM.insert fresh [a, b] im) cs (n - 1)
      | Just ia <- ma
      , Nothing <- mb =
          go xa xb fresh (Map.insert b ia acc) (IM.insertWith (<>) ia [b] im) cs (n - 1)
      | Just ib <- mb
      , Nothing <- ma =
          go xa xb fresh (Map.insert a ib acc) (IM.insertWith (<>) ib [a] im) cs (n - 1)
      | Just ia <- ma
      , Just ib <- mb
      , ia /= ib
      , acc' <- Map.fromList (map (,ia) (im IM.! ib)) <> acc
      , im' <- IM.insertWith (<>) ia (im IM.! ib) $ IM.delete ib im =
          go xa xb fresh acc' im' cs (n - 1)
      | otherwise = go x y fresh acc im cs (n - 1)
      where
        (a@(xa, _, _), b@(xb, _, _)) : cs = con
        ma = acc Map.!? a
        mb = acc Map.!? b

day8 :: IO (String, String)
day8 = do
  input <-
    map ((\[a, b, c] -> (read @Int a, read @Int b, read @Int c)) . splitOn ",")
      . lines
      <$> (readFile . (++ "/input/input8.txt") =<< getDataDir)
  let !finalAnsa =
        show
          . product
          . take 3
          . sortBy (comparing Down)
          . map length
          . IM.elems
          . fst
          $ connect input 1000
  let !finalAnsb =
        show $ snd $ connect input 1000
  pure (finalAnsa, finalAnsb)
