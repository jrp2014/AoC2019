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
                                                )
import qualified Data.Set                      as Set

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


-- | Find output destinations for each warp tile.
findLinks ::
  Map.Map String [Coord] {- ^ labeled tiles -} ->
  Map.Map Coord Coord    {- ^ warp links    -}
findLinks xs =
  Map.fromList
    do [p1,p2] <- Map.elems xs
       [(p1,p2), (p2,p1)]

solvePt1 input = (start, end, next start, findLinks portalCoords)
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


