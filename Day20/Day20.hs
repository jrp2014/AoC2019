{-# LANGUAGE ViewPatterns, BlockArguments#-}
module Day20 where

import           Data.Char                      ( isUpper )
import           Data.List                      ( elemIndices
                                                , tails
                                                , transpose
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes
                                                , maybeToList
                                                , fromJust
                                                )
import qualified Data.Set                      as Set

import           Algorithm.Search


type Coord = (Int, Int)
type Maze = Set.Set Coord -- passage points
type Portals = Map.Map (Int, Int) String

parse :: String -> (Maze, Portals)
parse (lines -> rows@(transpose -> cols)) = (maze, portals) where
  maze = Set.fromList
    [ (x, y) | (y, row) <- zip [0 ..] rows, x <- elemIndices '.' row ]

  match xy (a : b : '.' : _) | isUpper a, isUpper b = Just (xy + 2, [a, b])
  match xy ('.' : a : b : _) | isUpper a, isUpper b = Just (xy, [a, b])
  match _ _ = Nothing

  matchAll = catMaybes . zipWith match [0 ..] . tails

  portals =
    Map.fromList
      $  [ ((x, y), s) | (y, row) <- zip [0 ..] rows, (x, s) <- matchAll row ]
      ++ [ ((x, y), s) | (x, col) <- zip [0 ..] cols, (y, s) <- matchAll col ]


-- NB: This is not a universal solution ...
invertMap :: (Ord k, Ord v) => Map.Map k v -> Map.Map v [k]
invertMap m = Map.fromListWith (++) tuples
  where tuples = [ (value, [key]) | (key, value) <- Map.toList m ]

neighbours :: (Num a) => (a, a) -> [(a, a)]
neighbours (x, y) = [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]

solvePt1 :: String -> Int
solvePt1 input = length . fromJust $ bfs next (== end) start
 where

  (maze, portals) = parse input

  portalCoords :: Map.Map String [Coord]
  portalCoords = invertMap portals

  -- Only 1 start / end Coord
  [start]      = portalCoords Map.! "AA"
  [end  ]      = portalCoords Map.! "ZZ"

  next :: Coord -> [Coord] -- where you can get to in a single step
  next p = filter (`Set.member` maze) (neighbours p) -- adjacent passage
                                                     ++ do
    portal <- maybeToList $ portals Map.!? p -- where adjacent poortals take you
    filter (/= p) $ portalCoords Map.! portal

main :: IO ()
main = do
  inp <- readFile "input.txt"
  putStr "Part 1: "
  print $ solvePt1 inp

eg1 :: String
eg1 =
  "         A           \n\
  \         A           \n\
  \  #######.#########  \n\
  \  #######.........#  \n\
  \  #######.#######.#  \n\
  \  #######.#######.#  \n\
  \  #######.#######.#  \n\
  \  #####  B    ###.#  \n\
  \BC...##  C    ###.#  \n\
  \  ##.##       ###.#  \n\
  \  ##...DE  F  ###.#  \n\
  \  #####    G  ###.#  \n\
  \  #########.#####.#  \n\
  \DE..#######...###.#  \n\
  \  #.#########.###.#  \n\
  \FG..#########.....#  \n\
  \  ###########.#####  \n\
  \             Z       \n\
  \             Z       "

eg2 :: String
eg2 =
    "                   A               \n\
    \                   A               \n\
    \  #################.#############  \n\
    \  #.#...#...................#.#.#  \n\
    \  #.#.#.###.###.###.#########.#.#  \n\
    \  #.#.#.......#...#.....#.#.#...#  \n\
    \  #.#########.###.#####.#.#.###.#  \n\
    \  #.............#.#.....#.......#  \n\
    \  ###.###########.###.#####.#.#.#  \n\
    \  #.....#        A   C    #.#.#.#  \n\
    \  #######        S   P    #####.#  \n\
    \  #.#...#                 #......VT\n\
    \  #.#.#.#                 #.#####  \n\
    \  #...#.#               YN....#.#  \n\
    \  #.###.#                 #####.#  \n\
    \DI....#.#                 #.....#  \n\
    \  #####.#                 #.###.#  \n\
    \ZZ......#               QG....#..AS\n\
    \  ###.###                 #######  \n\
    \JO..#.#.#                 #.....#  \n\
    \  #.#.#.#                 ###.#.#  \n\
    \  #...#..DI             BU....#..LF\n\
    \  #####.#                 #.#####  \n\
    \YN......#               VT..#....QG\n\
    \  #.###.#                 #.###.#  \n\
    \  #.#...#                 #.....#  \n\
    \  ###.###    J L     J    #.#.###  \n\
    \  #.....#    O F     P    #.#...#  \n\
    \  #.###.#####.#.#####.#####.###.#  \n\
    \  #...#.#.#...#.....#.....#.#...#  \n\
    \  #.#####.###.###.#.#.#########.#  \n\
    \  #...#.#.....#...#.#.#.#.....#.#  \n\
    \  #.###.#####.###.###.#.#.#######  \n\
    \  #.#.........#...#.............#  \n\
    \  #########.###.###.#############  \n\
    \           B   J   C               \n\
    \           U   P   P               "

