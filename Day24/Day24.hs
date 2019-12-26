{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Day24 where

import           Control.Arrow                  ( (***) )
import           Control.Comonad                ( Comonad(..) )
import           Control.Comonad.Representable.Store
                                                ( Store
                                                , StoreT(..)
                                                , experiment
                                                , store
                                                )
import           Control.Concurrent
import           Data.Bool                      ( bool )
import           Data.Distributive              ( Distributive(..) )
import           Data.Functor.Compose           ( Compose(..) )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Functor.Rep               ( Representable(..)
                                                , distributeRep
                                                )
import qualified Data.Set                      as Set
import qualified Data.Vector                   as V

import           Test.Hspec


-- from https://chrispenner.ca/posts/conways-game-of-life
-- ... a neat approach using comonads, but it's not much fun
-- iterating through a grid

newtype VBounded a = VBounded (V.Vector a)
  deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
  distribute = distributeRep

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v V.! (i `mod` gridSize)
  tabulate desc = VBounded $ V.generate gridSize desc

type Grid a = Store (Compose VBounded VBounded) a
data Coord = C !Int !Int

gridSize :: Int
gridSize = 5

middle :: Coord
middle = C (gridSize `div` 2) (gridSize `div` 2)

inside :: Coord -> Bool
inside (C x y) = 0 <= x && 0 <= y && x < gridSize && y < gridSize

addCoord :: Coord -> Coord -> Coord
addCoord (C x y) (C x' y') = C (x + x') (y + y')

class    Neighbours a     where adjacents :: a -> [a]

instance Neighbours Coord where
  adjacents s = filter inside $ addCoord s <$> neighbourCoord

   where
    -- Offsets for the neighbouring 8 tiles, avoiding (0, 0) which is the cell itself
    neighbourCoord :: [Coord]
    neighbourCoord = [(C (-1) 0), (C 1 0), (C 0 (-1)), (C 0 1)]

instance Neighbours C3    where
  adjacents = cardinal3

-----------------------------------------------------------------------
-- 3-dimensional recursive board coordinates
------------------------------------------------------------------------

data C3 = C3 !Int !Int !Int
  deriving (Eq, Ord, Show)

to3 :: Coord -> C3
to3 (C y x) = C3 0 y x

cardinal3, left3, right3, above3, below3 :: C3 -> [C3]

cardinal3 c = concat [left3 c, right3 c, above3 c, below3 c]

left3 (C3 d y x) | x == 0         = [C3 (d - 1) 2 1]
                 | x == 3, y == 2 = [ C3 (d + 1) i 4 | i <- [0 .. 4] ]
                 | otherwise      = [C3 d y (x - 1)]

right3 (C3 d y x) | x == 4         = [C3 (d - 1) 2 3]
                  | x == 1, y == 2 = [ C3 (d + 1) i 0 | i <- [0 .. 4] ]
                  | otherwise      = [C3 d y (x + 1)]

below3 (C3 d y x) | y == 4         = [C3 (d - 1) 3 2]
                  | y == 1, x == 2 = [ C3 (d + 1) 0 i | i <- [0 .. 4] ]
                  | otherwise      = [C3 d (y + 1) x]

above3 (C3 d y x) | y == 0         = [C3 (d - 1) 1 2]
                  | y == 3, x == 2 = [ C3 (d + 1) 4 i | i <- [0 .. 4] ]
                  | otherwise      = [C3 d (y - 1) x]


--mkGrid :: [Coord] -> Grid Bool
mkGrid xs = store lookup (0 0) where lookup crd = crd `elem` xs

type Rule = Grid Bool -> Bool


part1Rule :: Neighbours a => Rule
part1Rule g =
  (numNeighboursAlive == 1) || (not alive && numNeighboursAlive == 2)
 where
  alive              = extract g
  neighbours         = experiment adjacents g
  numNeighboursAlive = length (filter id neighbours)

step :: Rule -> Grid Bool -> Grid Bool
step = extend

steps :: Rule -> Grid Bool -> [Grid Bool]
steps = iterate . step

--experiment :: (Coord -> [Coord]) -> Grid a -> [a]

render :: Grid Bool -> String
render (StoreT (Identity (Compose g)) _) =
  foldMap ((++ "\n") . foldMap (bool "." "#")) g

at :: [Coord] -> Coord -> [Coord]
at xs (x, y) = fmap ((+ x) *** (+ y)) xs

coordLines :: [String] -> [(Coord, Char)]
coordLines rows =
  [ ((y, x), z) | (y, row) <- zip [0 ..] rows, (x, z) <- zip [0 ..] row ]

bugCoord :: [String] -> [Coord]
bugCoord xs = [ k | (k, '#') <- coordLines xs ]

parse :: String -> Grid Bool
parse = mkGrid . bugCoord . lines


diversity :: Grid Bool -> Int
diversity (StoreT (Identity (Compose g)) _) =
  mult . foldMap (foldMap (bool [0] [1])) $ g
 where
  mult []      = 0
  mult (x : y) = x + 2 * mult y

findDup :: Ord a => [a] -> a
findDup = go Set.empty
 where
  go _ [] = error "no duplicates"
  go seen (x : xs) | Set.member x seen = x
                   | otherwise         = go (Set.insert x seen) xs


solvePt1 :: Grid Bool -> Int
solvePt1 grid = findDup $ map diversity (steps part1Rule grid)

loop :: (Grid Bool -> Grid Bool) -> Grid Bool -> IO ()
loop stepper g = do
--  putStr "\ESC[2J" -- Clear terminal screen
  putStrLn (render g)
  threadDelay tickTime
  loop stepper (stepper g)

main :: IO ()
main = do
  putStrLn "Part 1:"
  let start1 = parse input1
  print $ solvePt1 start1


tickTime :: Int
tickTime = 200000

test :: IO ()
test = do
  let start1 = parse eg1
  -- loop (step part1Rule) start1
  print $ diversity (parse eg2)
  print $ findDup $ map diversity (steps part1Rule start1)

input1 :: String
input1 = "###..\n\
  \#...#\n\
  \.#.##\n\
  \##.#.\n\
  \#.###"

eg1 :: String
eg1 = "....#\n\
  \#..#.\n\
  \#..##\n\
  \..#..\n\
  \#...."

eg2 :: String
eg2 = ".....\n\
  \.....\n\
  \.....\n\
  \#....\n\
  \.#..."
