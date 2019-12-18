{-# LANGUAGE TupleSections #-}
module Day17 where


import qualified Data.Sequence                 as Seq
import qualified Data.Map                      as M
import           Data.Char                      ( chr
                                                , ord
                                                )

import           Data.List                      ( inits
                                                , intercalate
                                                , unfoldr
                                                , stripPrefix
                                                )
import           Data.List.Split                ( splitOn )
import           Control.Monad                  ( guard )
import           Control.Applicative            ( empty )
import           Data.Foldable                  ( asum )

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


type Segment = Either Int Int

showSegment :: Segment -> String
showSegment (Left  n) = 'L' : ',' : show n
showSegment (Right n) = 'R' : ',' : show n

start :: Grid -> Coord
start grid = head [ k | (k, '^') <- M.toList grid ]

path :: Grid -> Coord -> Coord -> [Segment]
path grid location direction
  | '#' == at (addCoord (turnLeft direction) location) grid = go Left turnLeft
  | '#' == at (addCoord (turnRight direction) location) grid = go Right
                                                                  turnRight
  | otherwise = []
 where
  go cmd turn = cmd n : path grid segmentEnd direction'
   where
    direction' = turn direction
    steps      = takeWhile (\pos -> at pos grid == '#') . tail $ iterate
      (addCoord direction')
      location
    n          = length steps
    segmentEnd = last steps


findProgs :: Eq a => [a] -> [([a], [a], [a])]
findProgs p0 = do
  a <- validPrefix p0

  let withoutA = splitOn' a p0
  b <- case withoutA of
    []     -> empty              -- 'A' consumed everything, whoops
    bs : _ -> validPrefix bs

  let withoutB = splitOn' b =<< withoutA
  c <- case withoutB of
    []     -> empty              -- 'A' and 'B' consumed everything, whoops
    cs : _ -> validPrefix cs

  let withoutC = splitOn' c =<< withoutB
  guard $ null withoutC

  pure (a, b, c)
 where
    -- | Get all valid prefixes
  validPrefix = take 4 . filter (not . null) . inits
  -- | a version of splitOn that only returns non-empty lists
  splitOn' x = filter (not . null) . splitOn x


-- | Given an association list of subroutines and their "label", iteratively
-- chomp through a string replacing each occurence of the subroutine with the
-- label.
chomp :: Eq a => [([a], b)] -> [a] -> [b]
chomp progs = unfoldr go
  where go xs = asum [ (r, ) <$> stripPrefix prog xs | (prog, r) <- progs ]

data Prog = A | B | C deriving (Show)

solvePt2 :: Memory -> Grid -> Int
solvePt2 prog grid = last $ execute prog (solvePt2' grid)

solvePt2' :: Grid -> [Int]
solvePt2' grid = head $ map ord . unlines . solvePt2'' <$> findProgs p
 where
  p :: [Segment]
  p = path grid (start grid) (-1, 0)

  solvePt2'' :: ([Segment], [Segment], [Segment]) -> [String]
  solvePt2'' (a, b, c) = map
    (intercalate ",")
    [ [showProgs $ chomp [(a, A), (b, B), (c, C)] p]
    , showSegment <$> a
    , showSegment <$> b
    , showSegment <$> c
    , ["n"]
    ]

  showProgs :: [Prog] -> String
  showProgs = tail . init . show


main :: IO ()
main = do
  inp <- readFile "input.txt"
  let prog       = parse inp
  let gridString = map chr (execute prog [])
  let grid       = toGrid gridString
  putStr gridString
  putStr "Part 1: "
  print $ solvePt1 grid
  putStr "Part 2: "
  -- print $ solvePt2' grid
  let prog2 = Seq.update 0 2 prog
  --print . last $ execute prog2 input2
  -- print $ solvePt2' grid
  print $ solvePt2 prog2 grid

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
