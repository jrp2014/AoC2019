module Day13 where

import qualified Data.Sequence                 as S
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
solvePt2 sw = undefined
  where
    cabinet = mkMachine  (S.fromList []) sw

play :: [Int] -> Maybe Int -> Maybe Int -> Int -> Memory -> [Int]
play inputs ballX paddleX score m = last outZ
  where
   outA = execute m outZ'
   outZ = execute m outA'
  case execute m [0] of
    [] -> score
    (-1:0:score':rest) -> play rest ballX paddleX score' m
    (x : _ : 3 : rest) -> play rest ballX (Just x) score m
    (x : _ : 4 : rest) -> play rest (Just x) paddleX score m
    


main :: IO ()
main = do
  software <- readFile "input.txt"
  let psoftware = parse software
  print $ solvePt1 psoftware
  let p2software = S.update 0 2 psoftware -- 2 quareters inserted
  print "hi"
