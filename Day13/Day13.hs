module Day13 where

import qualified Data.Sequence                 as S
import           IntCode
import           Debug.Trace

data TileType = Empty | Wall | Block | Paddle | Ball deriving (Show, Eq, Enum)

type Tile = ((Int, Int), TileType)

parse :: String -> Memory
parse s = S.fromList . read $ '[' : s ++ "]"

solvePt1 :: Memory -> Int
solvePt1 sw = countBlocks $ execute sw []
 where
  countBlocks [] = 0
  countBlocks (_ : _ : t : rest) =
    (if toEnum t == Block then 1 else 0) + countBlocks rest


data GameState = GameState {score :: Int, ballX :: Maybe Int, paddleX :: Maybe Int, machine :: Machine}

solvePt2 :: Memory -> Int
solvePt2 sw = play initialGameState (repeat 0)
  where initialGameState = GameState 0 Nothing Nothing (mkMachine [] sw)



play :: GameState -> [Int] -> Int
play gs inputs = play' gs inputs outputs
 where
  outputs = execute (memory $ machine gs) inputs

  play' :: GameState -> [Int] -> [Int] -> Int
  play' gs' inputs' outputs' = case outputs' of
    [] -> play gs' inputs''
    (-1 : 0 : sc : rest) ->
      trace ("score=" ++ show sc) $ play' (gs' { score = sc }) inputs'' rest
    (x : _ : tile : rest) -> case toEnum tile of
      Ball   -> play' (gs' { ballX = Just x }) inputs'' rest
      Paddle -> play' (gs' { paddleX = Just x }) inputs'' rest
      _      -> play' gs' inputs'' rest
    where inputs'' = repeat $ joystickAI (paddleX gs') (ballX gs')

joystickAI :: Maybe Int -> Maybe Int -> Int
joystickAI px bx = case (px, bx) of
  (Just px, Just bx) -> signum $ bx - px
  (_      , _      ) -> 0

main :: IO ()
main = do
  software <- readFile "input.txt"
  let psoftware = parse software
  print $ solvePt1 psoftware
  let p2software = S.update 0 2 psoftware -- 2 quarters inserted
  print $ solvePt2 p2software

