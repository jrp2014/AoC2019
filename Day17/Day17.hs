module Day17 where


import qualified Data.Sequence                 as S
import qualified Data.Map                      as M
import           Data.Char                      ( chr )
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

at :: Coord -> Grid -> Char
at = M.findWithDefault '.'


parse :: String -> Memory
parse inp = S.fromList . read $ '[' : inp ++ "]"


toGrid :: String -> Grid
toGrid inp = M.fromList
  [ ((y, x), column)
  | (y, row   ) <- zip [0 ..] (lines inp)
  , (x, column) <- zip [0 ..] row
  ]

solvePt1 :: String -> Int
solvePt1 = solvePt1' . toGrid


solvePt1' :: Grid -> Int
solvePt1' grid = sum
  [ coordRow k * coordCol k
  | k <- M.keys grid
  , all (\x -> '#' == at x grid) (k : cardinal k)
  ]


main :: IO ()
main = do
  inp <- readFile "input.txt"
  let pinp = parse inp
  let grid   = map chr (execute pinp [])
  putStr grid
  putStr "Part 1: "
  print $ solvePt1 grid


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
      solvePt1 eg1 `shouldBe` 76
