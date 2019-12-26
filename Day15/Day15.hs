module Day15 where


import           Control.Arrow                  ( first
                                                , second
                                                )
import qualified Data.Sequence                 as S
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isNothing )
import           Data.List                      ( nubBy )


import           IntCode

type Coord = (Int, Int)

data Tile = Free | Wall | Oxygen deriving (Show, Eq)

type Grid = Map.Map Coord Tile

move :: Int -> Coord -> Coord
move 1 = second pred
move 2 = second succ
move 3 = first pred
move 4 = first succ

runRobotWithInput :: Memory -> [Int] -> (Grid, Coord)
runRobotWithInput mem input =
  let output = execute mem input
      state  = scanl trackRobot (Map.fromList [((0, 0), Free)], (0, 0))
        $ zip input output

      trackRobot :: (Grid, Coord) -> Coord -> (Grid, Coord)
      trackRobot (m, oldPos) (inp, outp) =
          let newPos =
                  let (x, y) = oldPos
                  in  case inp of -- only used if move was successful
                        1 -> (x, y + 1) -- north
                        2 -> (x, y - 1) -- south
                        3 -> (x - 1, y) -- west
                        4 -> (x + 1, y) -- east
          in  case outp of
                0 -> (Map.insert newPos Wall m, oldPos)
                1 -> (Map.insert newPos Free m, newPos)
                2 -> (Map.insert newPos Oxygen m, newPos)
  in  last . map snd $ zip input (tail state)


explore :: Memory -> [Grid]
explore mem =
  let
    neighbours :: Coord -> [Coord]
    neighbours (x, y) = [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]

    unmappedNeighbours :: Grid -> Coord -> [Coord]
    unmappedNeighbours m pos = filter (`Map.notMember` m) $ neighbours pos

    explore' :: (Grid, [([Int], Coord)]) -> (Grid, [([Int], Coord)])
    explore' (m, pathsAndEndpoints) =
      let
        -- Find paths which end in a position that has unmapped neighbours,
        -- then create forked paths from there.
        pathsToExplore :: [[Int]]
        pathsToExplore =
          map fst
            . filter (\(p, pos) -> not . null $ unmappedNeighbours m pos)
            $ pathsAndEndpoints
        newPaths :: [[Int]]
        newPaths =
          concatMap (\p -> map (\x -> p ++ [x]) [1 .. 4]) pathsToExplore

        -- Run the robot along the new paths, note the new map information and endpoints
        (newMaps, newEndpoints) =
          unzip . map (runRobotWithInput mem) $ newPaths

        -- Update the map with all new information
        newMap :: Grid
        newMap = foldr Map.union m newMaps

        -- Join paths and their endpoints, cull paths that end in the same endpoint
        newPathsAndEndpoints :: [([Int], Coord)]
        newPathsAndEndpoints =
          nubBy (\(_, pos1) (_, pos2) -> pos1 == pos2)
            $ zip newPaths newEndpoints
      in
        (newMap, newPathsAndEndpoints)
  in
    map fst . tail . iterate explore' $ (Map.empty, [([], (0, 0))])


main :: IO ()
main = do
  input <- readFile "input.txt"
  let mem = S.fromList $ read $ '[' : input ++ "]"
  --mapM_ (putStrLn . (++"\n\n") . mapToString) $ explore mem
  (print . (+ 1) . length)
    . takeWhile (\m -> Oxygen `notElem` Map.elems m)
    $ explore mem
