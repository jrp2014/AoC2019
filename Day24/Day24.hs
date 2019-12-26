
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
module Day24 where

import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S


import           Control.Arrow                  (  first, second )


type Coords = (Int, Int)

left, right, up, down :: Coords -> Coords
left = first pred
right = first succ
up = second pred
down = second succ


type Tile = (Int, Coords)

type Grid = Set Tile

size :: Int
size = 5

flatten :: [[a]] -> [((Int, Int), a)]
flatten rows =
  [ ((x, y), a) | (y, row) <- zip [0 ..] rows, (x, a) <- zip [0 ..] row ]

firstDuplicate :: Ord a => [a] -> a
firstDuplicate = go S.empty where
  go seen (x : xs) | x `S.member` seen = x
                   | otherwise         = go (S.insert x seen) xs

centre :: Coords
centre = (size `div` 2, size `div` 2)

inBounds :: Coords -> Bool
inBounds (x, y) = x >= 0 && x < size && y >= 0 && y < size

neighbours :: Tile -> [Tile]
neighbours (l, p) =
  [ (l, p') | d <- [left, right, up, down], let p' = d p, inBounds p' ]

recursiveNeighbours :: Tile -> [Tile]
recursiveNeighbours (l, p) = do
  (d, border) <-
    [(left, (pred size, )), (up, (, pred size)), (right, (0, )), (down, (, 0))]
  let p' = d p
  if
    | p' == centre -> [ (succ l, border i) | i <- [0 .. pred size] ]
    | inBounds p'  -> [(l, p')]
    | otherwise    -> [(pred l, d centre)]

applyRule :: (Tile -> [Tile]) -> Grid -> Grid
applyRule adjacents bugs =
  keysWhere (== 1) ( n `M.restrictKeys` bugs)
    `S.union` keysWhere (`elem` [1, 2]) ( n `M.withoutKeys` bugs)
 where
  n = M.fromListWith (+) [ (p, 1) | p <- foldMap adjacents bugs ]
  keysWhere p = M.keysSet . M.filter p

step :: (Tile -> [Tile]) -> Grid -> [Grid]
step = iterate . applyRule

diversity :: Grid -> Int
diversity = sum . map biodiversity . S.toList
  where biodiversity (_, (x, y)) = 2 ^ (x + y * size)

main :: IO ()
main = do
  let input = flatten $ lines input1
  let bugs  = S.fromList [ (0, p) | (p, '#') <- input ]

  putStr "Part 1: "
  print $ firstDuplicate $ diversity <$> iterate (applyRule neighbours) bugs

  putStr "Part 2: "
  print $ length $ iterate (applyRule recursiveNeighbours) bugs !! 200


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
