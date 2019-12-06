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
antecedents :: Orbits -> Planet -> [Planet]
antecedents orbits planet = case lookup planet orbits of
  Nothing     -> []
  Just parent -> planet : antecedents orbits parent

part1 :: Orbits -> Int
part1 orbits =
  sum [ length (antecedents orbits planet) | (planet, _) <- orbits ]

part2 :: Orbits -> Int
part2 orbits =
  length youantecedents + length sanantecedents - 2 * length commonantecedents
 where
  you               = fromJust $ lookup "YOU" orbits
  san               = fromJust $ lookup "SAN" orbits
  youantecedents    = antecedents orbits you
  sanantecedents    = antecedents orbits san
  commonantecedents = youantecedents `intersect` sanantecedents


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
