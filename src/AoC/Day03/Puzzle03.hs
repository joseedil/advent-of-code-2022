module AoC.Day03.Puzzle03 where

import Data.Char
import Data.Maybe

test :: String
test = "vJrwpWtwJgWrhcsFMMfFFhFp\n" ++
       "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n" ++
       "PmmdzqPrVvPwwTWBwg\n" ++
       "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n" ++
       "ttgJtRGJQctTZtZT\n" ++
       "CrZsJsPPZsGzwwsLwLmpwMDw\n"

firstLine :: String
firstLine = "vJrwpWtwJgWrhcsFMMfFFhFp"

file :: FilePath
file = "src/AoC/Day03/data.txt"


splitString :: String -> (String, String)
splitString s = splitAt (length s `div` 2) s

findElem :: String -> Char -> Maybe Char
findElem s c
  | elem c s = Just c
  | otherwise = Nothing

findCommon :: String -> String -> Char
findCommon left right = head $ mapMaybe (findElem left) right

priority :: Char -> Int
priority c
  | isUpper c = ord c - 64 + 26
  | otherwise = ord c - 96

answer1 :: IO Int
answer1 = sum . fmap (priority . uncurry findCommon . splitString) . lines <$> readFile file


-- Part 2

splitGroups :: [String] -> [[String]]
splitGroups [] = []
splitGroups s = take 3 s : splitGroups (drop 3 s)

findCommon3 :: [String] -> Char
findCommon3 (s1:s2:s3:[]) = head $ mapMaybe (findElem s1) . mapMaybe (findElem s2) $ s3
findCommon3 _ = error "invalid list"

answer2 :: IO Int
answer2 = sum . fmap (priority . findCommon3) . splitGroups . lines <$> readFile file
