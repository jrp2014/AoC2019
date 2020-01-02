{-# LANGUAGE TypeFamilies #-}

module Day18 where

import           Control.Arrow           hiding ( left
                                                , right
                                                )
import           Data.Array.IArray             as A
import           Data.Array.Unboxed
import           Data.Char
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.PriorityQueue.FingerTree as PQ
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as S

import           Test.Hspec


-- Naïm Favier's solution


type Coord = (Int, Int)

left, right, up, down :: Coord -> Coord
left = first pred
right = first succ
up = second pred
down = second succ

add :: Coord -> Coord -> Coord
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

ccw, cw :: Coord -> Coord
ccw (x, y) = (y, -x)
cw (x, y) = (-y, x)

flatten :: [[a]] -> [((Int, Int), a)]
flatten rows =
  [ ((x, y), a) | (y, row) <- zip [0 ..] rows, (x, a) <- zip [0 ..] row ]

count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\c e -> if p e then succ c else c) 0

counts :: (Foldable t, Ord a) => t a -> Map a Int
counts = foldl' (\m e -> M.insertWith (+) e 1 m) M.empty

findDuplicates :: Ord a => [a] -> [a]
findDuplicates = go S.empty where
  go seen (x : xs) | x `S.member` seen = x : xs'
                   | otherwise         = xs'
    where xs' = go (S.insert x seen) xs
  go _ [] = []

firstDuplicate :: Ord a => [a] -> a
firstDuplicate = head . findDuplicates

-- 
pickOne :: [a] -> [(a, [a])]
--pickOne l = [ (y, xs ++ ys) | (xs, y : ys) <- zip (inits l) (tails l) ]
pickOne []       = []
pickOne (x : xs) = (x, xs) : [ (y, x : ys) | (y, ys) <- pickOne xs ]

bfs :: Ord a => (a -> [a]) -> a -> [(a, Int)]
bfs = bfsOn id

bfsOn :: Ord b => (a -> b) -> (a -> [a]) -> a -> [(a, Int)]
bfsOn rep next start = go S.empty (Seq.singleton (start, 0)) where
  go _ Empty = []
  go seen ((n, d) :<| ps)
    | r `S.member` seen = go seen ps
    | otherwise = (n, d) : go
      (S.insert r seen)
      (ps <> Seq.fromList [ (n', d + 1) | n' <- next n ])
    where r = rep n

dijkstra :: Ord a => (a -> [(a, Int)]) -> a -> [(a, Int)]
dijkstra = dijkstraOn id

dijkstraOn :: Ord b => (a -> b) -> (a -> [(a, Int)]) -> a -> [(a, Int)]
dijkstraOn rep next start = go S.empty (PQ.singleton 0 start) where
  go seen q
    | Just ((d, n), q') <- minViewWithKey q
    = let r = rep n
      in  if r `S.member` seen
            then go seen q'
            else (n, d) : go
              (S.insert r seen)
              (PQ.union q' (PQ.fromList [ (d + c, n') | (n', c) <- next n ]))
    | otherwise
    = []

neighbours :: Coord -> [Coord]
neighbours p = [ d p | d <- [left, right, up, down] ]

diagonal :: Coord -> [Coord]
diagonal p = [ d p | d <- [left . up, left . down, right . up, right . down] ]

type Key = Char
type Door = Char
type Distance = Int

shortestPath :: UArray Coord Char -> Distance
shortestPath grid = head
  [ d
  | ((_, ks), d) <- dijkstra bigStep (start, S.empty)
  , length ks == length keyCoords
  ]
 where
  keyCoords :: [Coord]
  keyCoords = [ p | (p, c) <- A.assocs grid, isLower c ]

  start :: [Coord]
  start = sort [ p | (p, '@') <- A.assocs grid ]

  smallStep :: (Coord, S.Set Key) -> [(Coord, S.Set Key)]
  smallStep (p, ks) = do
    n <- neighbours p
    case grid ! n of
      '#'           -> [] -- Wall
      d | isUpper d -> return (n, S.insert (toLower d) ks) -- Door
      _             -> return (n, ks)

  distancesToKeys :: Map Coord [(Key, Coord, Distance, S.Set Door)]
  distancesToKeys = M.fromList [ (p, dists p) | p <- start ++ keyCoords ]
   where
    dists :: Coord -> [(Key, Coord, Int, S.Set Key)]
    dists from =
      [ (k, p, d, ds)
      | ((p, ds), d) <- bfsOn fst smallStep (from, S.empty)
      , let k = grid ! p
      , isLower k
      ] -- is a key

  bigStep :: ([Coord], S.Set Key) -> [(([Coord], S.Set Key), Distance)]
  bigStep (ps, ks) =
    [ ((Data.List.insert p' ps', S.insert k ks), d)
    | (p, ps')       <- pickOne ps
    , (k, p', d, ds) <- distancesToKeys M.! p
    , k `S.notMember` ks
    , ds `S.isSubsetOf` ks
    ]

solvePt1 :: String -> Distance
solvePt1 s = shortestPath grid
 where
  input           = lines s
  (width, height) = (genericLength (head input), genericLength input)
  grid            = array ((0, 0), (width - 1, height - 1)) (flatten input)

solvePt2 :: String -> Distance
solvePt2 s = shortestPath grid'
 where
  input           = lines s
  (width, height) = (genericLength (head input), genericLength input)
  centre          = (width `div` 2, height `div` 2)
  grid            = array ((0, 0), (width - 1, height - 1)) (flatten input)
  grid' =
    grid
      // [ (p, '#') | p <- centre : neighbours centre ]
      // [ (p, '@') | p <- diagonal centre ]

main :: IO ()
main = do
  s <- readFile "input.txt"
  putStr "Part 1: "
  print $ solvePt1 s
  putStr "Part 2: "
  print $ solvePt2 s


test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      solvePt1 eg1 `shouldBe` 8
    it "Example 2" $ do
      solvePt1 eg2 `shouldBe` 86
    it "Example 3" $ do
      solvePt1 eg3 `shouldBe` 132
    it "Example 4" $ do
      solvePt1 eg4 `shouldBe` 81
  describe "Part 2" $ do
    it "Example 5" $ do
      solvePt2 eg5 `shouldBe` 8
    it "Example 6" $ do
      solvePt2 eg6 `shouldBe` 24
    it "Example 7" $ do
      solvePt2 eg7 `shouldBe` 32
    it "Example 8" $ do
      solvePt2 eg8 `shouldBe` 72


eg1 :: String
eg1 = "#########\n\
 \#b.A.@.a#\n\
 \#########"

eg2 :: String
eg2 =
  "########################\n\
 \#f.D.E.e.C.b.A.@.a.B.c.#\n\
 \######################.#\n\
 \#d.....................#\n\
 \########################"

eg3 :: String
eg3 =
  "########################\n\
 \#...............b.C.D.f#\n\
 \#.######################\n\
 \#.....@.a.B.c.d.A.e.F.g#\n\
 \########################"

eg4 :: String
eg4 =
  "########################\n\
 \#@..............ac.GI.b#\n\
 \###d#e#f################\n\
 \###A#B#C################\n\
 \###g#h#i################\n\
 \########################"

eg5 :: String
eg5 =
  "#######\n\
  \#a.#Cd#\n\
  \##@#@##\n\
  \#######\n\
  \##@#@##\n\
  \#cB#Ab#\n\
  \#######"

eg6 :: String
eg6 =
  "###############\n\
  \#d.ABC.#.....a#\n\
  \######@#@######\n\
  \###############\n\
  \######@#@######\n\
  \#b.....#.....c#\n\
  \###############"

eg7 :: String
eg7 =
  "#############\n\
  \#DcBa.#.GhKl#\n\
  \#.###@#@#I###\n\
  \#e#d#####j#k#\n\
  \###C#@#@###J#\n\
  \#fEbA.#.FgHi#\n\
  \#############"

eg8 :: String
eg8 =
  "#############\n\
  \#g#f.D#..h#l#\n\
  \#F###e#E###.#\n\
  \#dCba@#@BcIJ#\n\
  \#############\n\
  \#nK.L@#@G...#\n\
  \#M###N#H###.#\n\
  \#o#m..#i#jk.#\n\
  \#############"
