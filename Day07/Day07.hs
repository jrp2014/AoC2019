module Day05 where

import qualified Data.Sequence                 as S
import           Data.Bool                      ( bool )
import           Test.Hspec
import           Data.List                      ( permutations )
import           Debug.Trace

type Memory = S.Seq Int

data Machine =
  Machine
    { pc :: Int -- program counter
    , memory :: Memory
    , input :: [Int]
    , output :: [Int] -- outputs are reversed
    } deriving Show

mkMachine :: [Int] -> Memory -> Machine
mkMachine inp mem = Machine { pc = 0, memory = mem, input = inp, output = [] }

run :: Machine -> Machine
run m@(Machine counter mem inp outp) = case opcode of
  -- +
  1 -> run m { pc = counter + 4, memory = set (param 3) (getP 1 + getP 2) mem }
  -- *
  2 -> run m { pc = counter + 4, memory = set (param 3) (getP 1 * getP 2) mem }

  -- input
  3 -> run m { pc     = counter + 2
             , memory = set (param 1) (head inp) mem
             , input  = tail inp
             }
  -- output
  4 -> run m { pc = counter + 2, output = getP 1 : outp }

  -- Jump if true
  5 -> run m { pc = bool (getP 2) (counter + 3) (getP 1 == 0) }
  -- Jump if false
  6 -> run m { pc = bool (getP 2) (counter + 3) (getP 1 /= 0) }

  -- less than
  7 -> run m { pc     = counter + 4
             , memory = set (param 3) (bool 0 1 (getP 1 < getP 2)) mem
             }
  -- equals
  8 -> run m { pc     = counter + 4
             , memory = set (param 3) (bool 0 1 (getP 1 == getP 2)) mem
             }

  -- halt
  99 -> m
  o  -> error ("Invalid opcode " ++ show o ++ " at " ++ show counter)

 where
  get :: Int -> Int
  get = S.index mem -- get directly

  -- set, lazily
  set :: Int -> Int -> Memory -> Memory
  --set index value memry = value `seq` S.update index value memry
  set index value memry = S.update index value memry

  -- The value of the ith parameter, where the 0th parameter is the opcode
  param :: Int -> Int
  param i = get (counter + i)

  -- the value of the positionth digit of n
  digit :: Int -> Int -> Int
  digit position n = n `div` (10 ^ position) `mod` 10

  opcode :: Int
  opcode = param 0 `mod` 100

  -- the mode of the positionth parameter (0 = indirect / position mode, 1 = immediate mode)
  mode :: Int -> Int
  mode position = digit (position + 1) (param 0)

  getP position = case mode position of
    0 -> get (param position)  -- indirect
    1 -> param position        -- direct
    p ->
      error $ "Invalid parameter mode " ++ show p ++ " at pc " ++ show counter

execute :: Memory -> [Int] -> [Int]
execute mem inputs = output . run $ mkMachine inputs mem

executeAmps :: Memory -> [Int] -> Int
executeAmps acs (a : rest) = head outLast
 where
  outA    = execute acs (a : 0 : outLast)
  outLast = foldr (\phase inputs -> execute acs (phase : inputs)) outA rest

executeAmps' :: Memory -> [Int] -> Int
executeAmps' acs [a, b, c, d, e] = head outE
 where
  outA = execute acs (a : 0 : outE)
  outB = execute acs (b : outA)
  outC = execute acs (c : outB)
  outD = execute acs (d : outC)
  outE = execute acs (e : outD)


--solve :: [Int] -> Int -> Int -> Int
solve prog p0 p1 = executeAmps (S.fromList prog) <$> phases
  where phases = permutations [p0 .. p1]


main :: IO ()
main = do
  rawProg <- readFile "input.txt"
  let prog = read $ '[' : rawProg ++ "]"
  putStr "Part 1: "
  print $ solve prog 0 4
  putStr "Part 2: "
  -- print $ solve prog 5 9
  print $ executeAmps' (S.fromList eg4) <$> permutations [5..9]
  print $ executeAmps' (S.fromList eg5) <$> permutations [5..9]



eg1 :: [Int]
eg1 = [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]

eg2 :: [Int]
eg2 =
  [ 3
  , 23
  , 3
  , 24
  , 1002
  , 24
  , 10
  , 24
  , 1002
  , 23
  , -1
  , 23
  , 101
  , 5
  , 23
  , 23
  , 1
  , 24
  , 23
  , 23
  , 4
  , 23
  , 99
  , 0
  , 0
  ]

eg3 :: [Int]
eg3 =
  [ 3
  , 31
  , 3
  , 32
  , 1002
  , 32
  , 10
  , 32
  , 1001
  , 31
  , -2
  , 31
  , 1007
  , 31
  , 0
  , 33
  , 1002
  , 33
  , 7
  , 33
  , 1
  , 33
  , 31
  , 31
  , 1
  , 32
  , 31
  , 31
  , 4
  , 31
  , 99
  , 0
  , 0
  , 0
  ]

eg4 :: [Int]
eg4 =
  [ 3
  , 26
  , 1001
  , 26
  , -4
  , 26
  , 3
  , 27
  , 1002
  , 27
  , 2
  , 27
  , 1
  , 27
  , 26
  , 27
  , 4
  , 27
  , 1001
  , 28
  , -1
  , 28
  , 1005
  , 28
  , 6
  , 99
  , 0
  , 0
  , 5
  ]

eg5 :: [Int]
eg5 =
  [ 3
  , 52
  , 1001
  , 52
  , -5
  , 52
  , 3
  , 53
  , 1
  , 52
  , 56
  , 54
  , 1007
  , 54
  , 5
  , 55
  , 1005
  , 55
  , 26
  , 1001
  , 54
  , -5
  , 54
  , 1105
  , 1
  , 12
  , 1
  , 53
  , 54
  , 53
  , 1008
  , 54
  , 0
  , 55
  , 1001
  , 55
  , 1
  , 55
  , 2
  , 53
  , 55
  , 53
  , 4
  , 53
  , 1001
  , 56
  , -1
  , 56
  , 1005
  , 56
  , 6
  , 99
  , 0
  , 0
  , 0
  , 0
  , 10
  ]


{-
test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      solve eg1 0 4 `shouldBe` 43210
    it "Example 2" $ do
      solve eg2 0 4 `shouldBe` 54321
    it "Example 3" $ do
      solve eg3 0 4 `shouldBe` 65210
  describe "Part 2" $ do
    it "Example 1" $ do
      solve eg4 5 9 `shouldBe` 139629729
    it "Example 2" $ do
      solve eg5 5 9 `shouldBe` 18216
-}
