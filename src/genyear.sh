#!/usr/bin/env bash

for i in {1..25}; do
  cat << EOF > Day$i.hs
module Day${i} where

import Paths_AOC2025 (getDataDir)

day${i} :: IO ()
day${i} = do
  -- input <- (readFile . (++ "/input/input${i}.txt") =<< getDataDir)
  putStrLn
    . ("day${i}a: " ++)
    . show
    $ ()
  putStrLn
    . ("day${i}b: " ++)
    . show
    $ ()
EOF

done
