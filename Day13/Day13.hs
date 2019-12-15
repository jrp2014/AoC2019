module Day13 where

import qualified Data.Sequence                 as S
import           Data.List.Split                ( chunksOf )

import           IntCode


data TileType = Empty | Wall | Block | Paddle | Ball deriving (Show, Eq, Enum)

type Tile = ((Int, Int), TileType)

parse :: String -> Memory
parse s = S.fromList . read $ '[' : s ++ "]"

solvePt1 :: Memory -> Int
solvePt1 sw = countBlocks $ execute sw []
 where
  countBlocks [] = 0
  countBlocks (_ : _ : t : rest) =
    (if toEnum t == Block then 1 else 0) + countBlocks rest

solvePt2 :: Memory -> Int
solvePt2 sw = last ins
 where
  ins  = joystickCommands outs 0 0
  outs = chunksOf 3 $ execute sw ins

-- from input triples, paddleX and score -> joystick commands (with the score as tail-end Charlie)
joystickCommands :: [[Int]] -> Int -> Int -> [Int]
joystickCommands [] _ score = [score]
joystickCommands ([x, _, 4] : rest) paddleX score =
  direction : joystickCommands rest paddleX score  -- ball update
  where direction = signum (x - paddleX)
joystickCommands ([-1, 0, score] : rest) paddleX _ =
  joystickCommands rest paddleX score
joystickCommands ([x, _, 3] : rest) _ score = joystickCommands rest x score  -- paddle update
joystickCommands (_ : rest) paddleX score = joystickCommands rest paddleX score


main :: IO ()
main = do
  software <- readFile "input.txt"
  let psoftware = parse software
  putStr "Part 1: "
  print $ solvePt1 psoftware
  let p2software = S.update 0 2 psoftware -- 2 quarters inserted
  putStr "Part 2: "
  print $ solvePt2 p2software

