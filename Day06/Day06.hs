module Day06 where

import           Data.List                      ( intersect )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromJust )

type Planet = String
type Orbits = [(Planet, Planet)]

parse :: String -> Orbits
parse = map (trans . splitOn ")") . lines
  where trans [parent, child] = (child, parent)


-- NB: Omits COM, so gives the result gives the number of orbits
parents :: Orbits -> Planet -> [Planet]
parents orbits planet = case lookup planet orbits of
  Nothing     -> []
  Just parent -> planet : parents orbits parent

part1 :: Orbits -> Int
part1 orbits = sum [ length (parents orbits planet) | (planet, _) <- orbits ]

part2 :: Orbits -> Int
part2 orbits = length youparents + length sanparents - 2 * length commonparents
 where
  you           = fromJust $ lookup "YOU" orbits
  san           = fromJust $ lookup "SAN" orbits
  youparents    = parents orbits you
  sanparents    = parents orbits san
  commonparents = youparents `intersect` sanparents


main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr "Part 1: "
  print $ part1 (parse input)
  putStr "Part 2: "
  print $ part2 (parse input)


eg1 :: String
eg1 =
  "COM)B\n\
      \B)C\n\
      \C)D\n\
      \D)E\n\
      \E)F\n\
      \B)G\n\
      \G)H\n\
      \D)I\n\
      \E)J\n\
      \J)K\n\
      \K)L"

eg2 :: String
eg2 =
  "COM)B\n\
      \B)C\n\
      \C)D\n\
      \D)E\n\
      \E)F\n\
      \B)G\n\
      \G)H\n\
      \D)I\n\
      \E)J\n\
      \J)K\n\
      \K)L\n\
      \K)YOU\n\
      \I)SAN"
