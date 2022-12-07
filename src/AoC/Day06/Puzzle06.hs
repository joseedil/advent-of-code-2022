module AoC.Day06.Puzzle06 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

test1 :: T.Text
test1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

test2 :: T.Text
test2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"

test3 :: T.Text
test3 = "nppdvjthqldpwncqszvftbrmjlhg"

test4 :: T.Text
test4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

test5 :: T.Text
test5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

file :: FilePath
file = "src/AoC/Day06/data.txt"

validate :: T.Text -> Bool
validate input = maybe True (\(c, cs) -> not (c `T.elem` cs) && validate cs) $ T.uncons input

processSignal :: (T.Text -> Bool) -> Int -> Int -> T.Text -> Int
processSignal validationFunction validationLength acc input
  | validationFunction $ T.take validationLength input = acc
  | otherwise = processSignal validationFunction validationLength (acc + 1) (T.tail input)

answer1 :: IO Int
answer1 = processSignal validate 4 4 <$> T.readFile file


-- Part 2

answer2 :: IO Int
answer2 = processSignal validate 14 14 <$> T.readFile file
