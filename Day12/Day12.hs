module Day12 where


import           Data.List                      ( elemIndex
                                                , foldl'
                                                , transpose
                                                )


import           Test.Hspec

data  Dim = Dim {pos :: !Int, vel ::  !Int}  deriving (Eq, Ord, Show)


type Moon = [Dim] -- make it easier to act on dimensions of lists of moons



parse :: String -> [Moon]
parse s =
  map
      ((\(x : y : z : _) -> [newDim (read x), newDim (read y), newDim (read z)])
      . words
      )
    . lines
    $ filter (`notElem` "<>xyz=,") s
 where
  newDim :: Int -> Dim
  newDim x = Dim x 0



solvePt1 :: String -> Int -> Int
solvePt1 s n = sum (map energy (transpose byDim))
 where
  moons = parse s
  byDim = [ iterate step moonDim !! n | moonDim <- transpose moons ]

  energy :: Moon -> Int
  energy m = ke * pe
   where
    pe = sum $ map (abs . pos) m
    ke = sum $ map (abs . vel) m

solvePt2 :: String -> Int
solvePt2 s = foldl' lcm 1 periods
 where
  moons = parse s

  periods :: [Int] -- by dimension
  periods = map (period . iterate step) (transpose moons)


applyVelocity :: Dim -> Dim
applyVelocity (Dim x dx) = Dim (x + dx) dx

-- | Apply gravity to the first particle based on the second.
applyGravity :: Dim -> Dim -> Dim
applyGravity (Dim x v) (Dim y _) = Dim x (v + signum (y-x))

-- | Single step of one-dimension
step :: [Dim] -> [Dim]
step ps = [ applyVelocity (foldl' applyGravity p ps) | p <- ps ]

period :: Eq a => [a] -> Int
period []       = error "repeatList: no cycle"
period (x : xs) = 1 + n where Just n = elemIndex x xs


main :: IO ()
main = do
  putStr "Part 1: "
  print $ solvePt1 input1 1000
  putStr "Part 2: "
  print $ solvePt2 input1

input1 :: String
input1 =
  "<x=-7, y=-8, z=9>\n\
  \<x=-12, y=-3, z=-4>\n\
  \<x=6, y=-17, z=-9>\n\
  \<x=4, y=-10, z=-6>"


test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      solvePt1 eg1 10 `shouldBe` 179

    it "Example 2" $ solvePt1 eg2 100 `shouldBe` 1940

  describe "Part 2" $ do
    it "Example 1" $ do
      solvePt2 eg1 `shouldBe` 2772

    it "Example 2" $ solvePt2 eg2 `shouldBe` 4686774924



eg1 :: String
eg1 =
  "<x=-1, y=0, z=2>\n\
  \<x=2, y=-10, z=-7>\n\
  \<x=4, y=-8, z=8>\n\
  \<x=3, y=5, z=-1>"

eg2 :: String
eg2 =
  "<x=-8, y=-10, z=0>\n\
  \<x=5, y=5, z=10>\n\
  \<x=2, y=-7, z=3>\n\
  \<x=9, y=-8, z=-3>"
