module Solve (solveAll) where

import Control.Concurrent
import Data.List (sort)
import Day1
import Day10
import Day11
import Day12
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9

solveAll :: IO ()
solveAll = do
  q <- newChan
  let days =
        zip
          [1 ..]
          [ day1,
            day2,
            day3,
            day4,
            day5,
            day6,
            day7,
            day8,
            day9,
            day10,
            day11,
            day12
          ]
  mapM_ (\(i, a) -> forkIO (a >>= writeChan q . (i,))) days
  getChanContents q >>= putStrLn . unlines . map f . sort . take 12
  where
    f (i, (a, b)) = "day" ++ show i ++ "a: " ++ a ++ "\nday" ++ show i ++ "b: " ++ b
