module Day10 where

import           Data.List                      ( maximumBy
                                                , (\\)
                                                , sortOn
                                                , delete
                                                )
import           Data.Ord                       ( comparing )
import           Test.Hspec


type Coord = (Int, Int)
type Asteroids = [Coord]

parse :: String -> Asteroids
parse s =
  [ (x, y) | (y, row) <- zip [0 ..] (lines s), (x, '#') <- zip [0 ..] row ]


solvePt1 :: Asteroids -> (Coord, Int)
solvePt1 asteroids = maximumBy (comparing snd) (solvePt1' <$> asteroids)
 where
  solvePt1' :: Coord -> (Coord, Int)
  solvePt1' asteroid = (asteroid, length $ visible asteroids asteroid)

  -- those asteroids visible from asteroid
  visible :: Asteroids -> Coord -> Asteroids
  visible asteroids asteroid = filter (isVisible asteroids asteroid) asteroids


solvePt2 :: Coord -> Asteroids -> Asteroids
solvePt2 asteroid asteroids = solvePt2' asteroid (delete asteroid asteroids)
 where
  solvePt2' :: Coord -> Asteroids -> Asteroids
  solvePt2' _        []        = []
  solvePt2' asteroid asteroids = zapped
    ++ solvePt2 asteroid (asteroids \\ zapped)
   where
    zapped :: Asteroids
    zapped = filter (isVisible asteroids asteroid)
      $ sortOn (angle . recentre asteroid) asteroids

    recentre :: Coord -> Coord -> Coord
    recentre (ox, oy) (x, y) = (x - ox, y - oy)

    angle :: Coord -> Double
    angle (x, y) = atan2 (-fromIntegral x) (fromIntegral y)

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
  let pinput = parse input
  putStr "Part 1: "
  let (laser, seen) = solvePt1 pinput
  print seen
  putStr "Part 2: "
  let (x, y) = solvePt2 laser pinput !! 199
  print $ 100 * x + y

test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      solvePt1 (parse eg1) `shouldBe` ((3, 4), 8)

    it "Example 2" $ do
      solvePt1 (parse eg2) `shouldBe` ((5, 8), 33)

    it "Example 3" $ do
      solvePt1 (parse eg3) `shouldBe` ((1, 2), 35)

    it "Example 4" $ do
      solvePt1 (parse eg4) `shouldBe` ((6, 3), 41)

    it "Example 5" $ do
      solvePt1 (parse eg5) `shouldBe` ((11, 13), 210)

  describe "Part 2" $ do
    let (laser, _) = solvePt1 (parse eg5)
    let peg5 = parse eg5
    it "Example 1" $ do
      solvePt2 laser peg5 !! 0 `shouldBe` (11, 12)

    it "Example 2" $ do
      solvePt2 laser peg5 !! 1 `shouldBe` (12, 1)

    it "Example 3" $ do
      solvePt2 laser peg5 !! 2 `shouldBe` (12, 2)

    it "Example 4" $ do
      solvePt2 laser peg5 !! 9 `shouldBe` (12,8)

    it "Example 5" $ do
      solvePt2 laser peg5 !! 19 `shouldBe` (16, 0)

    it "Example 6" $ do
      solvePt2 laser peg5 !! 49 `shouldBe` (16, 9)

    it "Example 7" $ do
      solvePt2 laser peg5 !! 99 `shouldBe` (10, 16)

    it "Example 8" $ do
      solvePt2 laser peg5 !! 198 `shouldBe` (9, 6)

    it "Example 9" $ do
      solvePt2 laser peg5 !! 199 `shouldBe` (8, 2)

    it "Example 10" $ do
      solvePt2 laser peg5 !! 200 `shouldBe` (10, 9)

    it "Example 11" $ do
      solvePt2 laser peg5 !! 298 `shouldBe` (11, 1)


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
