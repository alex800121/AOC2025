module Day7 where

import Paths_AOC2025 (getDataDir)

split beam splitter = f 0 [] beam splitter
 where
  f next acc (n : bs) (False : ss) = f 0 ((next + n) : acc) bs ss
  f next (a : acc) (n : bs) (True : ss) = f n (0 : (a + n) : acc) bs ss
  f _ acc _ _ = zipWith (\a b -> if a then 0 else b) splitter $ reverse acc

day7 :: IO (String, String)
day7 = do
  input <- map (map (/= '.')) . lines <$> (readFile . (++ "/input/input7.txt") =<< getDataDir)
  -- input <- map (map (/= '.')) . lines <$> (readFile . (++ "/input/test7.txt") =<< getDataDir)
  let beam = map (\b -> if b then 1 else 0) (head input)
      splitter = tail input
  let !finalAnsa =
        show
          . length
          . concatMap (filter id)
          . zipWith (zipWith (\a b -> a && b > 0)) (tail input)
          $ scanl split beam (tail input)
  let !finalAnsb =
        show
          . sum
          $ foldl' split beam (tail input)
  pure (finalAnsa, finalAnsb)
