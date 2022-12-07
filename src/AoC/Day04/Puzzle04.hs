module AoC.Day04.Puzzle04 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

test :: T.Text
test = "2-4,6-8\n" <>
       "2-3,4-5\n" <>
       "5-7,7-9\n" <>
       "2-8,3-7\n" <>
       "6-6,4-6\n" <>
       "2-6,4-8\n"


file :: FilePath
file = "src/AoC/Day04/data.txt"

parseLine :: T.Text -> [Int]
parseLine s = fmap (read . T.unpack) subStrings
  where
    subStrings = T.split (\c -> c == ',' || c == '-') s

contain :: [Int] -> Bool
contain [pi1, pf1, pi2, pf2] = (pi1 >= pi2 && pf1 <= pf2) ||
                               (pi1 <= pi2 && pf1 >= pf2)
contain _ = error "invalid input"

answer1 :: IO Int
answer1 =  sum . fmap (fromEnum . contain . parseLine) . T.lines <$> T.readFile file


-- Part 2

overlap :: [Int] -> Bool
overlap [pi1, pf1, pi2, pf2] = (pi2 >= pi1 && pi2 <= pf1) ||
                               (pi1 >= pi2 && pi1 <= pf2)
overlap _ = error "invalid input"

answer2 :: IO Int
answer2 =  sum . fmap (fromEnum . overlap . parseLine) . T.lines <$> T.readFile file
