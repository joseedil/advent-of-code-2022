module AoC.Day01.Puzzle01 where

import Data.List

test :: String
test = "123\n234\n345\n\n456\n\n567"

testLines :: [String]
testLines = lines test

file :: FilePath
file = "src/AoC/Day01/data.txt"

answer1 :: IO Int
answer1 = maxCalories 0 . lines <$> readFile file

answer1' :: IO Int
answer1' = maxCalories' 0 . lines <$> readFile file


-- First version

elfCalories :: [String] -> Int
elfCalories = sum . map (read :: String -> Int)

maxCalories :: Int -> [String] -> Int
maxCalories acc [] = acc
maxCalories acc ss =
  let (actual, next) = span (not . null) ss
  in if elfCalories actual > acc
     then maxCalories (elfCalories actual) (drop 1 next)
     else maxCalories acc (drop 1 next)


-- Refactoring

maxCalories' :: Int -> [String] -> Int
maxCalories' acc [] = acc
maxCalories' acc ss =
  let (actual, next) = span (not . null) ss
  in maxCalories (max acc $ elfCalories actual) (drop 1 next)


-- In trying to rewrite maxCalories' with a foldr I came with this other solution which I consider more elegant
groupElfs :: [String] -> [[String]]
groupElfs [] = []
groupElfs ss = actual : groupElfs (drop 1 next)
  where (actual, next) = span (not . null) ss

answer1'' :: IO Int
answer1'' = maximum . fmap elfCalories . groupElfs . lines <$> readFile file


-- The second part asks the calories of the top three elves. Just add
-- a few more things to the pipeline

answer2 :: IO Int
answer2 = sum . take 3 . sortBy (flip compare) . fmap elfCalories . groupElfs . lines <$> readFile file
