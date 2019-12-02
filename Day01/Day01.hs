module Day01 where

import           Test.Hspec


fuel :: Integer -> Integer
fuel mod = mod `div` 3 - 2

fuel' :: Integer -> Integer
fuel' = sum . takeWhile (>0) . tail . iterate fuel


parse :: String -> [Integer]
parse = map read . lines

test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      fuel 12 `shouldBe` 2
    it "Example 2" $ do
      fuel 14 `shouldBe` 2
    it "Example 3" $ do
      fuel 1969 `shouldBe` 654
    it "Example 4" $ do
      fuel 100756 `shouldBe` 33583
  describe "Part 1" $ do
    it "Example 1" $ do
      fuel' 14 `shouldBe` 2
    it "Example 2" $ do
      fuel' 1969 `shouldBe` 966
    it "Example 3" $ do
      fuel' 100756 `shouldBe` 50346


main :: IO ()
main = do
  input <- readFile "input"
  let pinput = parse input
  putStr "Part 1: "
  print . sum . map fuel $ pinput
  putStr "Part 2: "
  print . sum . map fuel' $ pinput
