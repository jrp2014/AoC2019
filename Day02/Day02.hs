module Day02 where

import qualified Data.Sequence                 as S
import           Data.List.Split                ( splitOn )
import           Test.Hspec


type Program = S.Seq Int

-- Machine is pogram counter + Programme
type Machine = (Int, Program)

parse :: String -> Program
parse = S.fromList . map read . splitOn ","


part1 :: Program
part1 = parse "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,6,23,2,6,23,27,2,27,9,31,1,5,31,35,1,35,10,39,2,39,9,43,1,5,43,47,2,47,10,51,1,51,6,55,1,5,55,59,2,6,59,63,2,63,6,67,1,5,67,71,1,71,9,75,2,75,10,79,1,79,5,83,1,10,83,87,1,5,87,91,2,13,91,95,1,95,10,99,2,99,13,103,1,103,5,107,1,107,13,111,2,111,9,115,1,6,115,119,2,119,6,123,1,123,6,127,1,127,9,131,1,6,131,135,1,135,2,139,1,139,10,0,99,2,0,14,0"


run :: Machine -> Machine
run p@(ic, prog) = case prog `S.index` ic of
  1  -> run (ic + 4, S.update (prog `S.index` (ic + 3)) (arith (+)) prog)
  2  -> run (ic + 4, S.update (prog `S.index` (ic + 3)) (arith (*)) prog)
  99 -> p
 where
  arith op =
    (prog `S.index` (prog `S.index` (ic + 1)))
      `op` (prog `S.index` (prog `S.index` (ic + 2)))

test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      (snd $ run (0, S.fromList [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]))
        `shouldBe` S.fromList [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
    it "Example 2" $ do
      (snd $ run (0, S.fromList [1,0,0,0,99]))
        `shouldBe` S.fromList [2,0,0,0,99]
    it "Example 3" $ do
      (snd $ run (0, S.fromList [2,3,0,3,99]))
        `shouldBe` S.fromList [2,3,0,6,99]
    it "Example 4" $ do
      (snd $ run (0, S.fromList [2,4,4,5,99,0]))
        `shouldBe` S.fromList [2,4,4,5,99,9801]
    it "Example 5" $ do
      (snd $ run (0, S.fromList [1,1,1,4,99,5,6,0,99]))
        `shouldBe` S.fromList [30,1,1,4,2,5,6,0,99]

main :: IO()
main = do
  let part1' = S.update 1 12 part1
  let part1'' = S.update 2 2 part1'
  print $ run (0, part1'')
