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
play gs inputs =  play' gs inputs outputs
 where
  outputs = execute (memory $ machine gs) inputs

  play' :: GameState -> [Int] -> [Int] -> Int
  play' gs' inputs' outputs' = case outputs' of
    --[] -> play gs' inputs''
    (-1 : 0 : sc : rest) ->
      trace ("score=" ++ show sc) $ play (gs' { score = sc }) inputs''
    (x : _ : tile : rest) -> case toEnum tile of
      Paddle -> play' (gs' { paddleX = Just x }) inputs'' rest
      Ball   -> play' (gs' { ballX = Just x }) inputs'' rest
      _      -> play' gs' inputs'' rest
    where inputs'' = repeat $ joystickAI (ballX gs') (paddleX gs')


runGame :: [Int] -> Int
runGame mem = let
    output = run (initialize mem) input

    -- keep track of the paddle position, ball position, score; possibly move the paddle
    gameAI :: ((Int, Int, Int), Maybe Int) -> [Maybe Int] -> ((Int, Int, Int), Maybe Int)
    gameAI ((ballx, padx, score), _) [Nothing,   Nothing,      Nothing] = ((ballx, padx,    score), Just . signum $ ballx - padx)
    gameAI ((ballx, padx,     _), _) [Just (-1), Just 0, Just newScore] = ((ballx, padx, newScore), Nothing)
    gameAI ((ballx,    _, score), _) [Just x,    Just y,        Just 3] = ((ballx,    x,    score), Nothing)
    gameAI ((    _, padx, score), _) [Just x,    Just y,        Just 4] = ((    x, padx,    score), Nothing)
    gameAI (               state, _)                                  _ = (                  state, Nothing)

    (gameState, input') = unzip . scanl gameAI ((0, 0, 0), Nothing) . chunksOf 3 . padNothings $ output
    input = map fromJust . filter isJust $ input'

    -- helper function; turns one Nothing into three Nothings so the chunks of input are always aligned
    padNothings :: [Maybe a] -> [Maybe a]
    padNothings []           = []
    padNothings (Nothing:xs) = [Nothing, Nothing, Nothing] ++ (padNothings xs)
    padNothings (x:xs)       = x:(padNothings xs)
  in
    (\(_, _, score) -> score) . last $ gameState




joystickAI :: Maybe Int -> Maybe Int -> Int
joystickAI ballx paddlex = case (ballx, paddlex) of
  (Just bx, Just px)
     | bx < px -> -1
     | bx > px -> 1
  _ -> 0

main :: IO ()
main = do
  software <- readFile "input.txt"
  let psoftware = parse software
  print $ solvePt1 psoftware
  let p2software = S.update 0 2 psoftware -- 2 quarters inserted
  print $ solvePt2 p2software

