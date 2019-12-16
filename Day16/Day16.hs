module Day16 where

import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                )
import           Test.Hspec

parse :: String -> [Int]
parse = map digitToInt . init -- drop final newline

display :: [Int] -> String
display = map intToDigit


fft :: [Int] -> [Int]
fft input =
  [ lastDigit $ sum $ zipWith (*) input (mkPattern i)
  | i <- [1 .. length input]
  ]

 where
  lastDigit :: Int -> Int
  lastDigit = (`mod` 10) . abs

  mkPattern :: Int -> [Int]
  mkPattern position = tail $ concatMap (replicate position) basePattern

  basePattern :: [Int]
  basePattern = cycle [0, 1, 0, -1]


solvePt1 :: [Int] -> String
solvePt1 input = take 8 . display . (!! 100) $ iterate fft input

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = parse input
  putStr "Part 1: "
  print $ solvePt1 pinput


eg1 :: String
eg1 = "12345678"

test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      take 5 (display <$> iterate fft (parse eg1))
        `shouldBe` ["12345678", "48226158", "34040438", "03415518", "01029498"]
    it "Example 2" $ do
      solvePt1 (parse "80871224585914546619083218645595") `shouldBe` "24176176"
    it "Example 3" $ do
      solvePt1 (parse "19617804207202209144916044189917") `shouldBe` "73745418"
    it "Example 4" $ do
      solvePt1 (parse "69317163492948606335995924319873") `shouldBe` "52432133"
