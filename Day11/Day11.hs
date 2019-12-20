module Day11 where

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

boundingBox :: [Coord] -> Maybe (Coord, Coord)
boundingBox t = case t of
  []          -> Nothing
  (y, x) : cs -> go y x y x cs
 where
  go loy lox hiy hix [] = Just ((loy, lox), (hiy, hix))
  go loy lox hiy hix ((y, x) : cs) =
    go (min loy y) (min lox x) (max hiy y) (max hix x) cs

drawCoords :: Map Coord Char -> String
drawCoords pixels = unlines
  [ [ pixel (y, x) | x <- [minx .. maxx] ] | y <- [miny .. maxy] ]
 where
  pixel c = M.findWithDefault ' ' c pixels
  Just ((miny, minx), (maxy, maxx)) = boundingBox (M.keys pixels)

-- | Compute the turn function given a robot's output.
turnTo
  -- | robot turn output
  :: Colour -> Coord -> Coord
turnTo 0 = turnLeft
turnTo 1 = turnRight
turnTo x = error ("Unexpected turn command: " ++ show x)

turnLeft, turnRight :: Coord -> Coord
turnLeft (y, x) = (-x, y)

turnRight (y, x) = (x, -y)

-- | Character representation of paint number.
paintChar :: Int -> Char
paintChar 0 = '.'
paintChar 1 = '#'
paintChar x = error ("Unexpected paint color: " ++ show x)

data HullState =
  HullState
    { location :: Coord
    , direction :: Coord
    , hull :: Hull
    } deriving Show

initialHullState :: HullState
initialHullState = HullState (0, 0) (-1, 0) M.empty

runRobot :: [Int] -> HullState -> [HullState]
runRobot []                        _ = []
runRobot (newColour : turn : rest) h = paintedHull : runRobot rest paintedHull
 where
  newDirection = turnTo turn (direction h)
  newLocation  = addCoord (location h) newDirection
  paintedHull  = h { location  = newLocation
                   , direction = newDirection
                   , hull      = M.insert (location h) newColour (hull h)
                   }

solve :: Memory -> Int -> Hull
solve prog initialColour = hull (last hulls)
 where
  commands = execute prog colours
  colours =
    initialColour
      : ((\h -> M.findWithDefault 0 (location h) (hull h)) <$> hulls)
  hulls = runRobot commands initialHullState

solvePt1 :: Memory -> Int
solvePt1 prog = M.size $ solve prog 0

solvePt2 :: Memory -> Hull
solvePt2 prog =  solve prog 1

render :: Hull -> IO ()
render = putStrLn .  drawCoords . fmap paintChar

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let pinput = parse inp
  putStrLn "Part 1: "
  print $ solvePt1 pinput
  render $ solve pinput 0
  putStrLn "Part 2: "
  render $ solvePt2 pinput
