module Day09 where

import           Data.Bool                      ( bool )
import qualified Data.Sequence                 as S
import           Test.Hspec

type Memory = S.Seq Int

data Machine
  = Machine
      { pc :: Int, -- program counter
        memory :: Memory,
        input :: [Int],
        output :: [Int],
        relBase :: Int
      }

mkMachine :: [Int] -> Memory -> Machine
mkMachine inp mem = Machine { pc      = 0
                            , memory  = mem S.>< S.replicate 4096 0
                            , input   = inp
                            , output  = []
                            , relBase = 0
                            }

--
run :: Machine -> Machine
run m@(Machine counter mem inp outp relB) = case opcode of
  -- (+)
  1 -> run m { pc = counter + 4, memory = setP 3 (getP 1 + getP 2) mem }

  -- (*)
  2 -> run m { pc = counter + 4, memory = setP 3 (getP 1 * getP 2) mem }

  -- input
  3 ->
    run m { pc = counter + 2, memory = setP 1 (head inp) mem, input = tail inp }
  -- output
  4 -> run m { pc = counter + 2, output = outp ++ [getP 1] }

  -- Jump if true
  5 -> run m { pc = bool (getP 2) (counter + 3) (getP 1 == 0) }

  -- Jump if false
  6 -> run m { pc = bool (getP 2) (counter + 3) (getP 1 /= 0) }

  -- less than
  7 ->
    run m { pc = counter + 4, memory = setP 3 (bool 0 1 (getP 1 < getP 2)) mem }

  -- equals
  8 ->
    run m { pc = counter + 4, memory = setP 3 (bool 0 1 (getP 1 == getP 2)) mem }

  -- adjust relative base
  9  -> run m { pc = counter + 2, relBase = relB + getP 1 }

  -- halt
  99 -> m

  o  -> error ("Invalid opcode " ++ show o ++ " at " ++ show counter)
 where
  get :: Int -> Int
  get = S.index mem -- get directly

  -- set, lazily
  setP :: Int -> Int -> Memory -> Memory
  --setP position value memry = value `seq` S.update (param position) value memry
  setP position value memry = case mode position of
    0 -> S.update (param position) value memry
    1 -> error "Cannot set directly"
    2 -> S.update (relB + param position) value memry
    p ->
      error $ "Invalid setP parameter mode " ++ show p ++ " at pc " ++ show counter

  -- The value of the ith parameter, where the 0th parameter is the opcode
  param :: Int -> Int
  param i = get (counter + i)

  opcode :: Int
  opcode = param 0 `mod` 100

  getP position = case mode position of
    0 -> get (param position) -- indirect
    1 -> param position -- direct
    2 -> get (relB + param position) -- relative
    p ->
      error $ "Invalid getP parameter mode " ++ show p ++ " at pc " ++ show counter

  -- the mode of the positionth parameter
  -- 0 = indirect / position mode
  -- 1 = immediate mode
  -- 2 = relative mode
  mode :: Int -> Int
  mode position = digit (position + 1) (param 0)
   where
    -- the value of the positionth digit of n
    digit :: Int -> Int -> Int
    digit pos n = n `div` (10 ^ pos) `mod` 10

execute :: Memory -> [Int] -> [Int]
execute mem inputs = output . run $ mkMachine inputs mem


main :: IO ()
main = do
  rawProg <- readFile "input.txt"
  let prog = S.fromList $ read $ '[' : rawProg ++ "]"
  putStr "Part 1: "
  print $ execute prog [1]
  putStr "Part 2: "
  print $ execute prog [2]


test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      execute (S.fromList eg1) [] `shouldBe` eg1
    it "Example 2" $ do
      execute (S.fromList eg2) [] `shouldBe` [1219070632396864]
    it "Example 3" $ do
      execute (S.fromList eg3) [] `shouldBe` [1125899906842624]


eg1 :: [Int]
eg1 =
  [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]

eg2 :: [Int]
eg2 = [1102, 34915192, 34915192, 7, 4, 7, 99, 0]

eg3 :: [Int]
eg3 = [104, 1125899906842624, 99]
