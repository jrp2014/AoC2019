{-# LANGUAGE RecordWildCards #-}
module Day15 where


import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as S


import           IntCode


-- glguy's solution, with my IntCode and Naïm Favier's bfs


type Coord = (Int, Int) -- row, column

north, south, east, west :: Coord -> Coord
north (y, x) = (y - 1, x)
south (y, x) = (y + 1, x)
west (y, x) = (y, x - 1)
east (y, x) = (y, x + 1)

-- north (1), south (2), west (3), and east (4)

origin :: Coord
origin = (0, 0)

bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id

bfsOn :: Ord b => (a -> b) -> (a -> [a]) -> a -> [a]
bfsOn rep next start = go S.empty (Seq.singleton start) where
  go _ Empty = []
  go seen (n :<| ps)
    | r `S.member` seen = go seen ps
    | otherwise         = n : go (S.insert r seen) (ps <> Seq.fromList (next n))
    where r = rep n


data SearchState = SearchState
  { onOxygen :: !Bool  -- ^ Is the robot currently on the oxygen
  , distance :: !Int   -- ^ Commands issued so far
  , location :: !Coord -- ^ robot's current location
  , machine  :: Machine -- ^ robot control program state
  } deriving (Eq, Ord, Show)

-- | Initial search state starting from assumed non-oxygen at the origin.
newSearchState
  :: Memory {- ^ intcode -}
  -> SearchState
newSearchState prog = SearchState False 0 origin (mkMachine 0 prog)

-- | Breadth-first exploration of the maze
explore :: SearchState -> [SearchState]
explore = bfsOn location singleStep

-- | Generate the list of single steps that can be taken from a particular position.
singleStep :: SearchState -> [SearchState]
singleStep SearchState {..} =
  [ SearchState (o == 2) (distance + 1) (move location) m'
  | (i, move) <- [(1, north), (2, south), (3, west), (4, east)]
  , let m' = run machine { input = i }
  , let o  = output m'
  , o > 0
  ] -- (0 = wall)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let mem = Seq.fromList $ read $ '[' : input ++ "]"
  putStr "Part 1: "
  let [part1] = filter onOxygen $ explore (newSearchState mem)
  print $ distance part1
  putStr "Part 2: "
  print (distance (last (explore part1 { distance = 0 })))
