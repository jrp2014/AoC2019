module Day02 where

import qualified Data.Sequence                 as S
import           Test.Hspec


type Memory = S.Seq Int

-- Machine is program counter + Memoryme
type Machine = (Int, Memory)


input :: Memory
input = S.fromList
  [ 1
  , 0
  , 0
  , 3
  , 1
  , 1
  , 2
  , 3
  , 1
  , 3
  , 4
  , 3
  , 1
  , 5
  , 0
  , 3
  , 2
  , 9
  , 1
  , 19
  , 1
  , 19
  , 6
  , 23
  , 2
  , 6
  , 23
  , 27
  , 2
  , 27
  , 9
  , 31
  , 1
  , 5
  , 31
  , 35
  , 1
  , 35
  , 10
  , 39
  , 2
  , 39
  , 9
  , 43
  , 1
  , 5
  , 43
  , 47
  , 2
  , 47
  , 10
  , 51
  , 1
  , 51
  , 6
  , 55
  , 1
  , 5
  , 55
  , 59
  , 2
  , 6
  , 59
  , 63
  , 2
  , 63
  , 6
  , 67
  , 1
  , 5
  , 67
  , 71
  , 1
  , 71
  , 9
  , 75
  , 2
  , 75
  , 10
  , 79
  , 1
  , 79
  , 5
  , 83
  , 1
  , 10
  , 83
  , 87
  , 1
  , 5
  , 87
  , 91
  , 2
  , 13
  , 91
  , 95
  , 1
  , 95
  , 10
  , 99
  , 2
  , 99
  , 13
  , 103
  , 1
  , 103
  , 5
  , 107
  , 1
  , 107
  , 13
  , 111
  , 2
  , 111
  , 9
  , 115
  , 1
  , 6
  , 115
  , 119
  , 2
  , 119
  , 6
  , 123
  , 1
  , 123
  , 6
  , 127
  , 1
  , 127
  , 9
  , 131
  , 1
  , 6
  , 131
  , 135
  , 1
  , 135
  , 2
  , 139
  , 1
  , 139
  , 10
  , 0
  , 99
  , 2
  , 0
  , 14
  , 0
  ]



-- TODO:: turn this into a fold!
run :: Machine -> Machine
run p@(ip, prog) = case get ip of
  1  -> run (ip + 4, S.update (get (ip + 3)) (arith (+)) prog)
  2  -> run (ip + 4, S.update (get (ip + 3)) (arith (*)) prog)
  99 -> p
 where
  get = S.index prog
  arith op = get (get (ip + 1)) `op` get (get (ip + 2))

execute :: Int -> Int -> Memory -> Int
execute noun verb mem = snd (run (0, mem'')) `S.index` 0
 where
  mem'  = S.update 1 noun mem
  mem'' = S.update 2 verb mem'


test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      snd (run (0, S.fromList [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]))
        `shouldBe` S.fromList [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
    it "Example 2" $ do
      snd (run (0, S.fromList [1, 0, 0, 0, 99]))
        `shouldBe` S.fromList [2, 0, 0, 0, 99]
    it "Example 3" $ do
      snd (run (0, S.fromList [2, 3, 0, 3, 99]))
        `shouldBe` S.fromList [2, 3, 0, 6, 99]
    it "Example 4" $ do
      snd (run (0, S.fromList [2, 4, 4, 5, 99, 0]))
        `shouldBe` S.fromList [2, 4, 4, 5, 99, 9801]
    it "Example 5" $ do
      snd (run (0, S.fromList [1, 1, 1, 4, 99, 5, 6, 0, 99]))
        `shouldBe` S.fromList [30, 1, 1, 4, 2, 5, 6, 0, 99]

main :: IO ()
main = do
  putStr "Part 1: "
  print $ execute 12 2 input

  putStr "Part 2: "

  print
    (head
      [ 100 * noun + verb
      | noun <- [0 .. 99]
      , verb <- [0 .. 99]
      , execute noun verb input == 19690720
      ]
    )


