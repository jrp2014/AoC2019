module Day05 where

import qualified Data.Sequence                 as S
import           Data.Bool                      ( bool )
import           Test.Hspec
import           Test.QuickCheck                ( property )

type Memory = S.Seq Int

data Machine =
  Machine
    { pc :: Int -- program counter
    , memory :: Memory
    , input :: [Int]
    , output :: [Int] -- outputs are reversed
    } deriving Show

initMachine :: [Int] -> Memory -> Machine
initMachine inp mem =
  Machine { pc = 0, memory = mem, input = inp, output = [] }

run :: Machine -> Machine
run m@(Machine counter mem inp outp) = case opcode of
  1 -> run m { pc = counter + 4, memory = set (param 3) (getP 1 + getP 2) mem }
  2 -> run m { pc = counter + 4, memory = set (param 3) (getP 1 * getP 2) mem }

  3 -> run m { pc     = counter + 2
             , memory = set (param 1) (head inp) mem
             , input  = tail inp
             }
  4 -> run m { pc = counter + 2, output = getP 1 : outp }

  5 -> run m { pc = bool (getP 2) (counter + 3) (getP 1 == 0) }
  6 -> run m { pc = bool (getP 2) (counter + 3) (getP 1 /= 0) }

  7 -> run m { pc     = counter + 4
             , memory = set (param 3) (bool 0 1 (getP 1 < getP 2)) mem
             }
  8 -> run m { pc     = counter + 4
             , memory = set (param 3) (bool 0 1 (getP 1 == getP 2)) mem
             }

  99 -> m
  o  -> error ("Invalid opcode " ++ show o ++ " at " ++ show counter)


 where
  get :: Int -> Int
  get = S.index mem -- get directly

  -- set, strictly
  set :: Int -> Int -> Memory -> Memory
  set index value memry = value `seq` S.update index value memry

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


execute :: Int -> Memory -> Int
execute i p = head . output . run $ initMachine [i] p

main :: IO ()
main = do
  putStr "Part 1; "
  print $ execute 1 prog
  putStr "Part 2; "
  print $ execute 5 prog

test :: IO ()
test = hspec $ do
  describe "Part 2" $ do
    it "Example 1a" $ property $ \n ->
      execute n (S.fromList [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8])
        == if n == 8 then 1 else 0
    it "Example 1b" $ do
      execute 8 (S.fromList [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]) `shouldBe` 1
    it "Example 2a" $ property $ \n ->
      execute n (S.fromList [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8])
        == if n < 8 then 1 else 0
    it "Example 2b" $ do
      execute 7 (S.fromList [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]) `shouldBe` 1
    it "Example 2c" $ do
      execute 8 (S.fromList [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]) `shouldBe` 0
    it "Example 2d" $ do
      execute 9 (S.fromList [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]) `shouldBe` 0
    it "Example 3a" $ property $ \n ->
      execute n (S.fromList [3, 3, 1108, -1, 8, 3, 4, 3, 99])
        == if n == 8 then 1 else 0
    it "Example 3b" $ do
      execute 8 (S.fromList [3, 3, 1108, -1, 8, 3, 4, 3, 99]) `shouldBe` 1
    it "Example 4a" $ property $ \n ->
      execute n (S.fromList [3, 3, 1107, -1, 8, 3, 4, 3, 99])
        == if n < 8 then 1 else 0
    it "Example 4b" $ do
      execute 7 (S.fromList [3, 3, 1107, -1, 8, 3, 4, 3, 99]) `shouldBe` 1
    it "Example 4c" $ do
      execute 8 (S.fromList [3, 3, 1107, -1, 8, 3, 4, 3, 99]) `shouldBe` 0
    it "Example 4d" $ do
      execute 9 (S.fromList [3, 3, 1107, -1, 8, 3, 4, 3, 99]) `shouldBe` 0
    it "Example 5a" $ property $ \n ->
      execute
          n
          (S.fromList [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9])
        == if n == 0 then 0 else 1
    it "Example 5b" $ property $ \n ->
      execute n (S.fromList [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1])
        == if n == 0 then 0 else 1
    it "Example 6" $ property $ \n ->
      execute
          n
          (S.fromList
            [ 3
            , 21
            , 1008
            , 21
            , 8
            , 20
            , 1005
            , 20
            , 22
            , 107
            , 8
            , 21
            , 20
            , 1006
            , 20
            , 31
            , 1106
            , 0
            , 36
            , 98
            , 0
            , 0
            , 1002
            , 21
            , 125
            , 20
            , 4
            , 20
            , 1105
            , 1
            , 46
            , 104
            , 999
            , 1105
            , 1
            , 46
            , 1101
            , 1000
            , 1
            , 20
            , 4
            , 20
            , 1105
            , 1
            , 46
            , 98
            , 99
            ]
          )
        == case compare n 8 of
             LT -> 999
             EQ -> 1000
             GT -> 1001

prog :: Memory
prog = S.fromList
  [ 3
  , 225
  , 1
  , 225
  , 6
  , 6
  , 1100
  , 1
  , 238
  , 225
  , 104
  , 0
  , 1001
  , 210
  , 88
  , 224
  , 101
  , -143
  , 224
  , 224
  , 4
  , 224
  , 1002
  , 223
  , 8
  , 223
  , 101
  , 3
  , 224
  , 224
  , 1
  , 223
  , 224
  , 223
  , 101
  , 42
  , 92
  , 224
  , 101
  , -78
  , 224
  , 224
  , 4
  , 224
  , 1002
  , 223
  , 8
  , 223
  , 1001
  , 224
  , 3
  , 224
  , 1
  , 223
  , 224
  , 223
  , 1101
  , 73
  , 10
  , 225
  , 1102
  , 38
  , 21
  , 225
  , 1102
  , 62
  , 32
  , 225
  , 1
  , 218
  , 61
  , 224
  , 1001
  , 224
  , -132
  , 224
  , 4
  , 224
  , 102
  , 8
  , 223
  , 223
  , 1001
  , 224
  , 5
  , 224
  , 1
  , 224
  , 223
  , 223
  , 1102
  , 19
  , 36
  , 225
  , 102
  , 79
  , 65
  , 224
  , 101
  , -4898
  , 224
  , 224
  , 4
  , 224
  , 102
  , 8
  , 223
  , 223
  , 101
  , 4
  , 224
  , 224
  , 1
  , 224
  , 223
  , 223
  , 1101
  , 66
  , 56
  , 224
  , 1001
  , 224
  , -122
  , 224
  , 4
  , 224
  , 102
  , 8
  , 223
  , 223
  , 1001
  , 224
  , 2
  , 224
  , 1
  , 224
  , 223
  , 223
  , 1002
  , 58
  , 82
  , 224
  , 101
  , -820
  , 224
  , 224
  , 4
  , 224
  , 1002
  , 223
  , 8
  , 223
  , 101
  , 3
  , 224
  , 224
  , 1
  , 223
  , 224
  , 223
  , 2
  , 206
  , 214
  , 224
  , 1001
  , 224
  , -648
  , 224
  , 4
  , 224
  , 102
  , 8
  , 223
  , 223
  , 101
  , 3
  , 224
  , 224
  , 1
  , 223
  , 224
  , 223
  , 1102
  , 76
  , 56
  , 224
  , 1001
  , 224
  , -4256
  , 224
  , 4
  , 224
  , 102
  , 8
  , 223
  , 223
  , 1001
  , 224
  , 6
  , 224
  , 1
  , 223
  , 224
  , 223
  , 1102
  , 37
  , 8
  , 225
  , 1101
  , 82
  , 55
  , 225
  , 1102
  , 76
  , 81
  , 225
  , 1101
  , 10
  , 94
  , 225
  , 4
  , 223
  , 99
  , 0
  , 0
  , 0
  , 677
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 1105
  , 0
  , 99999
  , 1105
  , 227
  , 247
  , 1105
  , 1
  , 99999
  , 1005
  , 227
  , 99999
  , 1005
  , 0
  , 256
  , 1105
  , 1
  , 99999
  , 1106
  , 227
  , 99999
  , 1106
  , 0
  , 265
  , 1105
  , 1
  , 99999
  , 1006
  , 0
  , 99999
  , 1006
  , 227
  , 274
  , 1105
  , 1
  , 99999
  , 1105
  , 1
  , 280
  , 1105
  , 1
  , 99999
  , 1
  , 225
  , 225
  , 225
  , 1101
  , 294
  , 0
  , 0
  , 105
  , 1
  , 0
  , 1105
  , 1
  , 99999
  , 1106
  , 0
  , 300
  , 1105
  , 1
  , 99999
  , 1
  , 225
  , 225
  , 225
  , 1101
  , 314
  , 0
  , 0
  , 106
  , 0
  , 0
  , 1105
  , 1
  , 99999
  , 8
  , 226
  , 677
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1005
  , 224
  , 329
  , 101
  , 1
  , 223
  , 223
  , 1008
  , 677
  , 677
  , 224
  , 1002
  , 223
  , 2
  , 223
  , 1006
  , 224
  , 344
  , 1001
  , 223
  , 1
  , 223
  , 107
  , 226
  , 677
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1005
  , 224
  , 359
  , 1001
  , 223
  , 1
  , 223
  , 1108
  , 677
  , 677
  , 224
  , 1002
  , 223
  , 2
  , 223
  , 1006
  , 224
  , 374
  , 101
  , 1
  , 223
  , 223
  , 1107
  , 677
  , 677
  , 224
  , 1002
  , 223
  , 2
  , 223
  , 1006
  , 224
  , 389
  , 101
  , 1
  , 223
  , 223
  , 108
  , 226
  , 677
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1006
  , 224
  , 404
  , 101
  , 1
  , 223
  , 223
  , 7
  , 677
  , 677
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1006
  , 224
  , 419
  , 101
  , 1
  , 223
  , 223
  , 108
  , 677
  , 677
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1006
  , 224
  , 434
  , 1001
  , 223
  , 1
  , 223
  , 7
  , 226
  , 677
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1006
  , 224
  , 449
  , 1001
  , 223
  , 1
  , 223
  , 108
  , 226
  , 226
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1005
  , 224
  , 464
  , 101
  , 1
  , 223
  , 223
  , 8
  , 226
  , 226
  , 224
  , 1002
  , 223
  , 2
  , 223
  , 1006
  , 224
  , 479
  , 101
  , 1
  , 223
  , 223
  , 1008
  , 226
  , 226
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1005
  , 224
  , 494
  , 1001
  , 223
  , 1
  , 223
  , 1008
  , 677
  , 226
  , 224
  , 1002
  , 223
  , 2
  , 223
  , 1005
  , 224
  , 509
  , 101
  , 1
  , 223
  , 223
  , 7
  , 677
  , 226
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1006
  , 224
  , 524
  , 101
  , 1
  , 223
  , 223
  , 1007
  , 677
  , 226
  , 224
  , 1002
  , 223
  , 2
  , 223
  , 1006
  , 224
  , 539
  , 1001
  , 223
  , 1
  , 223
  , 1108
  , 677
  , 226
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1005
  , 224
  , 554
  , 1001
  , 223
  , 1
  , 223
  , 8
  , 677
  , 226
  , 224
  , 1002
  , 223
  , 2
  , 223
  , 1005
  , 224
  , 569
  , 101
  , 1
  , 223
  , 223
  , 1108
  , 226
  , 677
  , 224
  , 1002
  , 223
  , 2
  , 223
  , 1005
  , 224
  , 584
  , 101
  , 1
  , 223
  , 223
  , 1107
  , 677
  , 226
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1006
  , 224
  , 599
  , 101
  , 1
  , 223
  , 223
  , 107
  , 226
  , 226
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1006
  , 224
  , 614
  , 1001
  , 223
  , 1
  , 223
  , 107
  , 677
  , 677
  , 224
  , 1002
  , 223
  , 2
  , 223
  , 1005
  , 224
  , 629
  , 1001
  , 223
  , 1
  , 223
  , 1107
  , 226
  , 677
  , 224
  , 1002
  , 223
  , 2
  , 223
  , 1006
  , 224
  , 644
  , 101
  , 1
  , 223
  , 223
  , 1007
  , 677
  , 677
  , 224
  , 102
  , 2
  , 223
  , 223
  , 1006
  , 224
  , 659
  , 1001
  , 223
  , 1
  , 223
  , 1007
  , 226
  , 226
  , 224
  , 1002
  , 223
  , 2
  , 223
  , 1006
  , 224
  , 674
  , 1001
  , 223
  , 1
  , 223
  , 4
  , 223
  , 99
  , 226
  ]
