
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
module Day24 where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S


import           Control.Arrow                  ( first
                                                , second
                                                )


type Coords = (Int, Int)

left, right, up, down :: Coords -> Coords
left = first pred
right = first succ
up = second pred
down = second succ


type Tile = (Int, Coords)

type Tiles = Set Tile


size :: Int
size = 5

centre :: Coords
centre = (size `div` 2, size `div` 2)

inside :: Coords -> Bool -- in bounds?
inside (x, y) = x >= 0 && x < size && y >= 0 && y < size

flatten :: [[a]] -> [((Int, Int), a)]
flatten rows =
  [ ((x, y), a) | (y, row) <- zip [0 ..] rows, (x, a) <- zip [0 ..] row ]

firstDuplicate :: (Ord a, Show a) => [a] -> a
firstDuplicate = go S.empty where
  go seen (x : xs) | x `S.member` seen = x
                   | otherwise         = go (S.insert x seen) xs
  go seen [] = error $ "Cannot find any duplicates. Seen: " ++ show seen


neighbours :: Tile -> Tiles
neighbours (l, p) = S.fromList
  [ (l, p') | move <- [left, right, up, down], let p' = move p, inside p' ]

recursiveNeighbours :: Tile -> Tiles
recursiveNeighbours (l, p) = S.fromList $ do
  (move, border) <-
    [(left, (pred size, )), (up, (, pred size)), (right, (0, )), (down, (, 0))]
  let p' = move p
  if
    | p' == centre -> [ (succ l, border i) | i <- [0 .. pred size] ]
    | inside p'    -> [(l, p')]
    | otherwise    -> [(pred l, move centre)]


applyRule :: (Tile -> Tiles) -> Tiles -> Tiles
applyRule adjacents tiles = S.filter rule $ S.unions (S.map adjacents tiles)
 where
  rule k = 1 == n || 2 == n && S.notMember k tiles
    where n = S.size $ adjacents k `S.intersection` tiles

steps :: (Tile -> Tiles) -> Tiles -> [Tiles]
steps = iterate . applyRule


diversity :: Tiles -> Int
diversity = sum . S.map biodiversity
  where biodiversity (_, (x, y)) = 2 ^ (x + y * size)

main :: IO ()
main = do
  let input = flatten $ lines input1
  let tiles  = S.fromList [ (0, p) | (p, '#') <- input ]

  putStr "Part 1: "
  print $ firstDuplicate $ diversity <$> steps neighbours tiles

  putStr "Part 2: "
  print $ length $ steps recursiveNeighbours tiles !! 200


input1 :: String
input1 = "###..\n\
  \#...#\n\
  \.#.##\n\
  \##.#.\n\
  \#.###"

eg1 :: String
eg1 = "....#\n\
  \#..#.\n\
  \#..##\n\
  \..#..\n\
  \#...."

eg2 :: String
eg2 = ".....\n\
  \.....\n\
  \.....\n\
  \#....\n\
  \.#..."
