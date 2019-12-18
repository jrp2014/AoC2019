module Day17 where


import qualified Data.Sequence                 as Seq
import qualified Data.Map                      as M
import           Data.Char                      ( chr
                                                , ord
                                                )
import           Data.List                      ( sortOn
                                                , tails
                                                , inits
                                                )

import           IntCode
import           Test.Hspec



type Coord = (Int, Int)
type Grid = M.Map Coord Char

coordRow, coordCol :: Coord -> Int
coordRow (row, _) = row
coordCol (_, col) = col

above, below, left, right :: Coord -> Coord
above (y, x) = (y - 1, x)
below (y, x) = (y + 1, x)
left (y, x) = (y, x - 1)
right (y, x) = (y, x + 1)

cardinal :: Coord -> [Coord]
cardinal c = c `seq` [above c, left c, right c, below c]

turnLeft, turnRight :: Coord -> Coord
turnLeft (y, x) = (-x, y)
turnRight (y, x) = (x, -y)

addCoord :: Coord -> Coord -> Coord
addCoord (y, x) (v, u) = (y + v, x + u)

at :: Coord -> Grid -> Char
at = M.findWithDefault '.'


parse :: String -> Memory
parse inp = Seq.fromList . read $ '[' : inp ++ "]"


toGrid :: String -> Grid
toGrid inp = M.fromList
  [ ((y, x), column)
  | (y, row   ) <- zip [0 ..] (lines inp)
  , (x, column) <- zip [0 ..] row
  ]


solvePt1 :: Grid -> Int
solvePt1 grid = sum
  [ coordRow k * coordCol k
  | k <- M.keys grid
  , all (\x -> '#' == at x grid) (k : cardinal k)
  ]


data Direction = U | D | L | R
  deriving (Eq, Ord, Show)

type Segment = (Direction, Int)


start :: Grid -> Coord
start grid = head [ k | (k, '^') <- M.toList grid ]

path :: Grid -> Coord -> Coord -> [Segment]
path grid location direction
  | '#' == at (addCoord (turnLeft direction) location) grid = go L turnLeft
  | '#' == at (addCoord (turnRight direction) location) grid = go R turnRight
  | otherwise = []
 where
  go cmd turn = (cmd, n) : path grid segmentEnd direction'
   where
    direction' = turn direction
    steps      = takeWhile (\pos -> at pos grid == '#') . tail $ iterate
      (addCoord direction')
      location
    n          = length steps
    segmentEnd = last steps

part2 :: [Int]
part2 =
  map ord
    . unlines
    $ [ "B,B,C,A,C,A,C,A,C,B"
      , "R,10,L,12,L,12"
      , "R,10,L,12,R,6"
      , "R,6,R,10,R,12,R,6"
      , "n"
      ]


main :: IO ()
main = do
  inp <- readFile "input.txt"
  let pinp       = parse inp
  let gridString = fmap chr (execute pinp [])
  let grid       = toGrid gridString
  putStr gridString
  putStr "Part 1: "
  print $ solvePt1 grid
  putStr "Part 2: "
  let ppath = path grid (start grid) (-1, 0)
  -- print ppath
  let pinp2 = Seq.update 0 2 pinp
  print .last $ execute pinp2 part2

eg1 :: String
eg1 =
  "..#..........\n\
  \..#..........\n\
  \#######...###\n\
  \#.#...#...#.#\n\
  \#############\n\
  \..#...#...#..\n\
  \..#####...^.."

test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      solvePt1 (toGrid eg1) `shouldBe` 76
