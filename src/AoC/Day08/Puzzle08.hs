{-# LANGUAGE RecordWildCards #-}
module Aoc.Day08.Puzzle08 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

test1 :: T.Text
test1 = "30373\n" <>
        "25512\n" <>
        "65332\n" <>
        "33549\n" <>
        "35390\n"

file :: FilePath
file = "src/AoC/Day08/data.txt"

type TreeSize = Int
data Forest = Forest { forestWidth :: Int,
                       forestHeight :: Int,
                       forestData :: V.Vector TreeSize }
            deriving (Eq, Show)

parseForest :: T.Text -> Forest
parseForest t = Forest { forestWidth = width
                       , forestHeight = height
                       , forestData = dat }
  where lines = T.lines t
        width = T.length . head $ lines
        height = length lines
        dat = V.replicate (width * height) 0 V.// indices 0 [] t

        indices :: Int -> [(Int, TreeSize)] -> T.Text -> [(Int, TreeSize)]
        indices inPos acc input
          | T.null input = reverse acc
          | otherwise =
              if T.head input == '\n'
              then indices inPos acc $ T.tail input
              else indices (inPos + 1) ((inPos, read [T.head input]) : acc) (T.tail input)

treeSizeAt :: Forest -> Int -> Int -> TreeSize
treeSizeAt forest@Forest{..} x y = forestData V.! index
  where index = y * forestWidth + x

isLeftEdge :: Forest -> Int -> Int -> Bool
isLeftEdge Forest{..} x _ = x == 0

isRightEdge :: Forest -> Int -> Int -> Bool
isRightEdge Forest{..} x _ = x == forestWidth - 1

isTopEdge :: Forest -> Int -> Int -> Bool
isTopEdge Forest{..} _ y = y == 0

isBottomEdge :: Forest -> Int -> Int -> Bool
isBottomEdge Forest{..} _ y = y == forestHeight - 1

isEdge :: Forest -> Int -> Int -> Bool
isEdge forest x y = or $ (\f -> f forest x y) <$> [isLeftEdge, isRightEdge, isTopEdge, isBottomEdge]

leftTrees :: Forest -> Int -> Int -> [TreeSize]
leftTrees forest@Forest{..} x y = worker forest x y 1 []
  where worker forest x y pos acc
          | x - pos < 0 = acc
          | otherwise = worker forest x y (pos + 1) $ treeSizeAt forest (x - pos) y : acc

rightTrees :: Forest -> Int -> Int -> [TreeSize]
rightTrees forest@Forest{..} x y = worker forest x y 1 []
  where worker forest x y pos acc
          | x + pos >= forestWidth = acc
          | otherwise = worker forest x y (pos + 1) $ treeSizeAt forest (x + pos) y : acc

topTrees :: Forest -> Int -> Int -> [TreeSize]
topTrees forest@Forest{..} x y = worker forest x y 1 []
  where worker forest x y pos acc
          | y - pos < 0 = acc
          | otherwise = worker forest x y (pos + 1) $ treeSizeAt forest x (y - pos) : acc

bottomTrees :: Forest -> Int -> Int -> [TreeSize]
bottomTrees forest@Forest{..} x y = worker forest x y 1 []
  where worker forest x y pos acc
          | y + pos >= forestHeight = acc
          | otherwise = worker forest x y (pos + 1) $ treeSizeAt forest x (y + pos) : acc

isVisible :: Forest -> Int -> Int -> Bool
isVisible forest@Forest{..} x y = or $ isVisible' . (\f -> f forest x y) <$> [leftTrees, rightTrees, topTrees, bottomTrees]
  where isVisible' [] = True
        isVisible' sizes = and $ (< treeSizeAt forest x y) <$> sizes

countVisibles :: Forest -> Int
countVisibles forest@Forest{..} = sum $ fmap (\(x,y) -> fromEnum $ isVisible forest x y) coords
  where coords = (,) <$> [0 .. forestWidth - 1] <*> [0 .. forestHeight - 1]

answer1 :: IO Int
answer1 = countVisibles . parseForest <$> T.readFile file

-- Part 2

forest = parseForest test1

leftDistance :: Forest -> Int -> Int -> Int
leftDistance forest@Forest{..} x y = worker 0 1
  where worker acc pos
          | isLeftEdge forest x y = 0
          | x - pos < 0 = acc
          | treeSizeAt forest (x - pos) y >= treeSizeAt forest x y = acc + 1
          | otherwise = worker (acc + 1) (pos + 1)

rightDistance :: Forest -> Int -> Int -> Int
rightDistance forest@Forest{..} x y = worker 0 1
  where worker acc pos
          | isRightEdge forest x y = 0
          | x + pos >= forestWidth = acc
          | treeSizeAt forest (x + pos) y >= treeSizeAt forest x y = acc + 1
          | otherwise = worker (acc + 1) (pos + 1)

topDistance :: Forest -> Int -> Int -> Int
topDistance forest@Forest{..} x y = worker 0 1
  where worker acc pos
          | isTopEdge forest x y = 0
          | y - pos < 0 = acc
          | treeSizeAt forest x (y - pos) >= treeSizeAt forest x y = acc + 1
          | otherwise = worker (acc + 1) (pos + 1)

bottomDistance :: Forest -> Int -> Int -> Int
bottomDistance forest@Forest{..} x y = worker 0 1
  where  worker acc pos
           | isBottomEdge forest x y = 0
           | y + pos >= forestHeight = acc
           | treeSizeAt forest x (y + pos) >= treeSizeAt forest x y = acc + 1
           | otherwise = worker (acc + 1) (pos + 1)

treeScore :: Forest -> Int -> Int -> Int
treeScore forest x y = product $ (\f -> f forest x y) <$> [leftDistance, rightDistance, topDistance, bottomDistance]

maxScore :: Forest -> Int
maxScore forest@Forest{..} = maximum $ fmap (uncurry $ treeScore forest) coords
  where
    coords = (,) <$> [0 .. forestWidth - 1] <*> [0 .. forestHeight - 1]

answer2 :: IO Int
answer2 = maxScore . parseForest <$> T.readFile file
