module Day19 where

import Data.List (find)
import Data.Sequence as S
import IntCode

parse :: String -> Memory
parse s = S.fromList . read $ '[' : s ++ "]"

raster :: Int -> Char
raster 0 = '.'
raster 1 = '#'
raster _ = '?'

solvePt2 :: (Int -> Int -> Bool) -> Int -> Int -> Int
solvePt2 isBeingPulled x0 y
  | isBeingPulled (x + 99) (y - 99) = x * 10000 + y - 99 -- top left corner is also in beam
  | otherwise = solvePt2 isBeingPulled x (y + 1) -- search next row, from the left beam edge in the previous row
  where
    Just x = find (`isBeingPulled` y) [x0 ..] -- find the first x on the left of the beam for the current row

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let pinput = parse inp

  let inBeam x y = head $ execute pinput [x, y] -- 0 or 1

  putStrLn "Part 1: "
  putStr $ unlines [map (raster . (`inBeam` y)) [0 .. 49] | y <- [0 .. 49]]
  print $ sum [inBeam x y | x <- [0 .. 49], y <- [0 .. 49]]

  putStr "Part 2: "
  let isBeingPulled x y = 1 == inBeam x  y
  print $ solvePt2 isBeingPulled 0 100
