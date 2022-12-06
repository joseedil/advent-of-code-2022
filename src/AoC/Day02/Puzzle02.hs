module AoC.Day02.Puzzle02 where

test :: String
test = "A Y\nB X\nC Z"

testLines :: [String]
testLines = lines test

file :: FilePath
file = "src/AoC/Day02/data.txt"

data Shape = Rock | Paper | Scissors deriving (Show, Eq)

instance Ord Shape where
  Rock `compare` Rock = EQ
  Rock `compare` Paper = LT
  Rock `compare` Scissors = GT

  Paper `compare` Rock = GT
  Paper `compare` Paper = EQ
  Paper `compare` Scissors = LT

  Scissors `compare` Rock = LT
  Scissors `compare` Paper = GT
  Scissors `compare` Scissors= EQ


readShape :: Char -> Shape
readShape 'A' = Rock
readShape 'B' = Paper
readShape 'C' = Scissors
readShape 'X' = Rock
readShape 'Y' = Paper
readShape 'Z' = Scissors
readShape _   = error "Can't recognize shape"

shapePoints :: Shape -> Int
shapePoints Rock = 1
shapePoints Paper = 2
shapePoints Scissors = 3

matchPoints :: Shape -> Shape -> Int
matchPoints opponent me
  -- lost
  | opponent > me  = 0 + shapePoints me
  -- draw
  | opponent == me = 3 + shapePoints me
  -- win
  | otherwise      = 6 + shapePoints me

parseMatch :: String -> (Shape, Shape)
parseMatch s = (opponent, me)
  where
    opponent = readShape $ head s
    me = readShape $ s !! 2  -- ugly, but works

answer1 :: IO Int
answer1 = sum . fmap (uncurry matchPoints . parseMatch) . lines <$> readFile file


-- Part 2

readShape' :: Char -> Shape
readShape' 'A' = Rock
readShape' 'B' = Paper
readShape' 'C' = Scissors

myMove :: Char -> Shape -> Shape
myMove 'X' Rock = Scissors
myMove 'X' Paper = Rock
myMove 'X' Scissors = Paper
myMove 'Y' Rock = Rock
myMove 'Y' Paper = Paper
myMove 'Y' Scissors = Scissors
myMove 'Z' Rock = Paper
myMove 'Z' Paper = Scissors
myMove 'Z' Scissors = Rock

parseMatch' :: String -> (Shape, Shape)
parseMatch' s = (opponent, me)
  where
    opponent = readShape' $ head s
    me = myMove (s !! 2) opponent

answer2 :: IO Int
answer2 = sum . fmap (uncurry matchPoints . parseMatch') . lines <$> readFile file
