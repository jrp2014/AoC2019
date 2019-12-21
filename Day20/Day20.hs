{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
module Day20 where

import           Algorithm.Search
import           Control.Monad                  ( guard )
import           Data.Char                      ( isUpper )
import           Data.Functor                   ( ($>) )
import           Data.List                      ( elemIndices
                                                , tails
                                                , transpose
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , maybeToList
                                                )
import qualified Data.Set                      as Set
import           Test.Hspec

type Coord = (Int, Int)
type Maze = Set.Set Coord -- passage points
type Portals = Map.Map (Int, Int) String

parse :: String -> (Maze, Portals, Map.Map String [Coord], Coord, Coord)
parse (lines -> rows@(transpose -> cols)) =
  (maze, portals, portalCoords, start, end) where
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


  portalCoords :: Map.Map String [Coord]
  portalCoords = invertMap portals

  -- Only 1 start / end Coord
  [start]      = portalCoords Map.! "AA"
  [end  ]      = portalCoords Map.! "ZZ"

  -- NB: This is not a universal solution ...
  invertMap :: (Ord k, Ord v) => Map.Map k v -> Map.Map v [k]
  invertMap m = Map.fromListWith (++) tuples
    where tuples = [ (value, [key]) | (key, value) <- Map.toList m ]

neighbours :: (Num a) => (a, a) -> [(a, a)]
neighbours (x, y) = [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]

solvePt1 :: String -> Int
solvePt1 input = length . fromJust $ bfs next (== end) start
 where

  (maze, portals, portalCoords, start, end) = parse input

  next :: Coord -> [Coord] -- where you can get to in a single step
  next p = filter (`Set.member` maze) (neighbours p) -- adjacent passage
                                                     ++ do
    portal <- maybeToList $ portals Map.!? p -- where adjacent poortals take you
    filter (/= p) $ portalCoords Map.! portal

solvePt2 :: String -> Int
solvePt2 input = length . fromJust $ bfs next (== (end, 0)) (start, 0)
 where

  (maze, portals, portalCoords, start, end) = parse input

  (xs  , ys  )                              = unzip $ Set.toList maze
  (minX, maxX)                              = (minimum xs, maximum xs)
  (minY, maxY)                              = (minimum ys, maximum ys)

  next :: (Coord, Int) -> [(Coord, Int)] -- where you can get to in a single step, including level
  next (p@(x, y), level) =
    map (, level) (filter (`Set.member` maze) (neighbours p)) -- adjacent passage
                                                              ++ do
      portal <- maybeToList $ portals Map.!? p -- where adjacent poortals take you
      q      <- filter (/= p) $ portalCoords Map.! portal
      if x `elem` [minX, maxX] || y `elem` [minY, maxY] -- if p on an outside edge ?
        then guard (level > 0) $> (q, level - 1) -- go in a level, if you can
        else guard (2 * level < Map.size portals) $> (q, level + 1)

main :: IO ()
main = do
  inp <- readFile "input.txt"
  putStr "Part 1: "
  print $ solvePt1 inp
  putStr "Part 2: "
  print $ solvePt2 inp


test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      solvePt1 eg1 `shouldBe` 23
    it "Example 2" $ do
      solvePt1 eg2 `shouldBe` 58

  describe "Part 2" $ do
    it "Example 3" $ do
      solvePt2 eg3 `shouldBe` 396


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


eg3 :: String
eg3 =
  "             Z L X W       C                 \n\
  \             Z P Q B       K                 \n\
  \  ###########.#.#.#.#######.###############  \n\
  \  #...#.......#.#.......#.#.......#.#.#...#  \n\
  \  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  \n\
  \  #.#...#.#.#...#.#.#...#...#...#.#.......#  \n\
  \  #.###.#######.###.###.#.###.###.#.#######  \n\
  \  #...#.......#.#...#...#.............#...#  \n\
  \  #.#########.#######.#.#######.#######.###  \n\
  \  #...#.#    F       R I       Z    #.#.#.#  \n\
  \  #.###.#    D       E C       H    #.#.#.#  \n\
  \  #.#...#                           #...#.#  \n\
  \  #.###.#                           #.###.#  \n\
  \  #.#....OA                       WB..#.#..ZH\n\
  \  #.###.#                           #.#.#.#  \n\
  \CJ......#                           #.....#  \n\
  \  #######                           #######  \n\
  \  #.#....CK                         #......IC\n\
  \  #.###.#                           #.###.#  \n\
  \  #.....#                           #...#.#  \n\
  \  ###.###                           #.#.#.#  \n\
  \XF....#.#                         RF..#.#.#  \n\
  \  #####.#                           #######  \n\
  \  #......CJ                       NM..#...#  \n\
  \  ###.#.#                           #.###.#  \n\
  \RE....#.#                           #......RF\n\
  \  ###.###        X   X       L      #.#.#.#  \n\
  \  #.....#        F   Q       P      #.#.#.#  \n\
  \  ###.###########.###.#######.#########.###  \n\
  \  #.....#...#.....#.......#...#.....#.#...#  \n\
  \  #####.#.###.#######.#######.###.###.#.#.#  \n\
  \  #.......#.......#.#.#.#.#...#...#...#.#.#  \n\
  \  #####.###.#####.#.#.#.#.###.###.#.###.###  \n\
  \  #.......#.....#.#...#...............#...#  \n\
  \  #############.#.#.###.###################  \n\
  \               A O F   N                     \n\
  \               A A D   M                     "

