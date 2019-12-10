module Day10 where

import           Data.List                      ( maximumBy )
import           Data.Ord                       ( comparing )
import           Test.Hspec


type Coord = (Int, Int)
type Asteroids = [Coord]

parse :: String -> Asteroids
parse s =
  [ (x, y) | (y, row) <- zip [0 ..] (lines s), (x, '#') <- zip [0 ..] row ]


solve :: Asteroids -> (Coord, Int)
solve asteroids = maximumBy (comparing snd) (solve' <$> asteroids)
 where
  solve' :: Coord -> (Coord, Int) -- no of asteroids visible from asteroid
  solve' asteroid =
    (asteroid, length $ filter (isVisible asteroids asteroid) asteroids)


isVisible :: Asteroids -> Coord -> Coord -> Bool
isVisible _ a1 a2 | a1 == a2              = False -- an asteroid is not visible from itself
isVisible asteroids (a1x, a1y) (a2x, a2y) = and
  [ (a2x + stepx * i, a2y + stepy * i) `notElem` asteroids -- no obstructing asteroid here
  | i <- [1 .. steps - 1]
  ]
 where
  dx    = a1x - a2x
  dy    = a1y - a2y

  steps = gcd dx dy

  stepx = dx `div` steps
  stepy = dy `div` steps


main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr "Part 1: "
  print $ solve (parse input)

test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      solve (parse eg1) `shouldBe` ((3, 4), 8)

    it "Example 2" $ do
      solve (parse eg2) `shouldBe` ((5, 8), 33)

    it "Example 3" $ do
      solve (parse eg3) `shouldBe` ((1, 2), 35)

    it "Example 4" $ do
      solve (parse eg4) `shouldBe` ((6, 3), 41)

    it "Example 5" $ do
      solve (parse eg5) `shouldBe` ((11, 13), 210)


eg1 :: String
eg1 = ".#..#\n\
      \.....\n\
      \#####\n\
      \....#\n\
      \...##"

eg2 :: String
eg2 = "......#.#.\n\
      \#..#.#....\n\
      \..#######.\n\
      \.#.#.###..\n\
      \.#..#.....\n\
      \..#....#.#\n\
      \#..#....#.\n\
      \.##.#..###\n\
      \##...#..#.\n\
      \.#....####"

eg3 :: String
eg3 = "#.#...#.#.\n\
      \.###....#.\n\
      \.#....#...\n\
      \##.#.#.#.#\n\
      \....#.#.#.\n\
      \.##..###.#\n\
      \..#...##..\n\
      \..##....##\n\
      \......#...\n\
      \.####.###."

eg4 :: String
eg4 = ".#..#..###\n\
      \####.###.#\n\
      \....###.#.\n\
      \..###.##.#\n\
      \##.##.#.#.\n\
      \....###..#\n\
      \..#.#..#.#\n\
      \#..#.#.###\n\
      \.##...##.#\n\
      \.....#.#.."

eg5 :: String
eg5 = ".#..##.###...#######\n\
      \##.############..##.\n\
      \.#.######.########.#\n\
      \.###.#######.####.#.\n\
      \#####.##.#.##.###.##\n\
      \..#####..#.#########\n\
      \####################\n\
      \#.####....###.#.#.##\n\
      \##.#################\n\
      \#####.##.###..####..\n\
      \..######..##.#######\n\
      \####.##.####...##..#\n\
      \.#####..#.######.###\n\
      \##...#.##########...\n\
      \#.##########.#######\n\
      \.####.#.###.###.#.##\n\
      \....##.##.###..#####\n\
      \.#.#.###########.###\n\
      \#.#.#.#####.####.###\n\
      \###.##.####.##.#..##"
