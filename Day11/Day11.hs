module Day11 where

import           Data.List                      ( find )
import           Data.Map                      as M
import           Data.Sequence                 as S
import           IntCode

parse :: String -> Memory
parse s = S.fromList . read $ '[' : s ++ "]"

type Colour = Int

type Coord = (Int, Int)

type Hull = M.Map Coord Colour

addCoord :: Coord -> Coord -> Coord
addCoord (y, x) (v, u) = (y + v, x + u)

-- | Compute the turn function given a robot's output.
turnTo
  ::
  -- | robot turn output
     Colour -> Coord -> Coord
turnTo 0 = turnLeft
turnTo 1 = turnRight
turnTo x = error ("Unexpected turn command: " ++ show x)

turnLeft, turnRight :: Coord -> Coord
turnLeft (y, x) = (-x, y)
turnRight (y, x) = (x, -y)

-- | Character representation of paint number.
paintChar :: Int -> Char
paintChar 0 = '░'
paintChar 1 = '█'
paintChar x = error ("Unexpected paint color: " ++ show x)

runRobot :: [Colour] -> Coord -> Coord -> Hull -> Hull
runRobot []                        _        _         hull = hull
runRobot (turn : newColour : rest) location direction hull = runRobot
  rest
  newLocation
  newDirection
  paintedHull
 where
  newDirection = turnTo turn direction
  newLocation  = addCoord location newDirection
  paintedHull  = M.insert location newColour hull

solvePt2 prog = runRobot colours (0, 0) (-1, 0) M.empty
  where
    (colour, direction)  = execute prog [0:colours]

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let pinput = parse inp
  putStrLn "Part 1: "
  putStr "Part 2: "
