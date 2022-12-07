module AoC.Day05.Puzzle05 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

test :: T.Text
test =
  "    [D]    \n" <>
  "[N] [C]    \n" <>
  "[Z] [M] [P]\n" <>
  " 1   2   3 \n" <>
  "\n" <>
  "move 1 from 2 to 1\n" <>
  "move 3 from 1 to 3\n" <>
  "move 2 from 2 to 1\n" <>
  "move 1 from 1 to 2\n"

file :: FilePath
file = "src/AoC/Day05/data.txt"

-- Stack operations

type Stack = [Char]

emptyStack :: Stack
emptyStack = []

pop :: Stack -> Stack
pop = tail

push :: Stack -> Char -> Stack
push s c = c:s

stackTop :: Stack -> Char
stackTop = head

move :: Stack -> Stack -> (Stack, Stack)
move (c:cs) dest = (cs, c:dest)
move _ _ = error "empty origin stack"

moveN :: Int -> Stack -> Stack -> (Stack, Stack)
moveN 0 orig dest = (orig, dest)
moveN n orig dest = uncurry (moveN (n - 1)) $ move orig dest


-- parsing

stackConfig :: [T.Text] -> [T.Text]
stackConfig = takeWhile (/= "")

numberOfStacks :: [T.Text] -> Int
numberOfStacks = maximum . fmap (read . T.unpack)

instructions :: [T.Text] -> [T.Text]
instructions = tail . dropWhile (/= "")

parseChunk :: T.Text -> Maybe Char
parseChunk s
  | s `T.index` 0 == '['
  , s `T.index` 2 == ']' = Just $ s `T.index` 1
  | otherwise = Nothing

parseInstruction :: T.Text -> (Int, Int, Int)
parseInstruction s = (qty, orig, dest)
  where
    tokens = T.splitOn " " s
    qty = read . T.unpack $ tokens !! 1
    orig = read . T.unpack $ tokens !! 3
    dest = read . T.unpack $ tokens !! 5


emptyStacks :: Int -> V.Vector Stack
emptyStacks n = V.replicate n emptyStack

updateStack :: Stack -> Maybe Char -> Stack
updateStack s = maybe s (push s)

updateStacks :: [Maybe Char] -> V.Vector Stack -> V.Vector Stack
updateStacks mcs stacks = V.zipWith updateStack stacks mcsVect
  where mcsVect = V.fromList mcs

initialStacks :: T.Text -> V.Vector Stack
initialStacks input = foldr updateStacks stacks initialCrates
  where
    ts = stackConfig . T.lines $ input
    stacks = emptyStacks . numberOfStacks . T.chunksOf 4 $ last ts
    initialCrates = fmap parseChunk . T.chunksOf 4 <$> init ts

moveCrates :: ((Int, Int, Int) -> V.Vector Stack -> V.Vector Stack) -> T.Text -> V.Vector Stack -> V.Vector Stack
moveCrates execInstruction input stacks = foldl (flip execInstruction) stacks moves
  where
    moves = fmap parseInstruction . instructions . T.lines $ input

execInstruction :: (Int, Int, Int) -> V.Vector Stack -> V.Vector Stack
execInstruction (qty, orig, dest) stacks = stacks V.// updates
  where updates = [(orig - 1, s1), (dest - 1, s2)]
        (s1, s2) = moveN qty (stacks V.! (orig - 1)) (stacks V.! (dest - 1))

getTops :: V.Vector Stack -> String
getTops = V.toList . fmap stackTop

answer1 :: IO String
answer1 = fmap (\input -> getTops $ moveCrates execInstruction input (initialStacks input)) (T.readFile file)


-- Part 2

moveN' :: Int -> Stack -> Stack -> (Stack, Stack)
moveN' 0 orig dest = (orig, dest)
moveN' n orig dest = (finalOrig, finalDest)
  where finalOrig = drop n orig
        finalDest = take n orig <> dest

execInstruction' :: (Int, Int, Int) -> V.Vector Stack -> V.Vector Stack
execInstruction' (qty, orig, dest) stacks = stacks V.// updates
  where updates = [(orig - 1, s1), (dest - 1, s2)]
        (s1, s2) = moveN' qty (stacks V.! (orig - 1)) (stacks V.! (dest - 1))


answer2 :: IO String
answer2 = fmap (\input -> getTops $ moveCrates execInstruction' input (initialStacks input)) (T.readFile file)
