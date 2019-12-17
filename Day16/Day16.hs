module Day16 where

import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                , isDigit
                                                )
import           Data.List                      ( scanl'
                                                , unfoldr
                                                , iterate'
                                                )
import           Control.Monad                  ( when
                                                , join
                                                )
import           Data.Tuple                     ( swap )
import           Test.Hspec

parse :: String -> [Int]
parse = map digitToInt . filter isDigit

display :: [Int] -> String
display = map intToDigit


fft :: [Int] -> [Int]
fft input =
  [ lastDigit $ sum $ zipWith (*) input (mkPattern i)
  | i <- [1 .. length input]
  ]

 where
  mkPattern :: Int -> [Int]
  mkPattern position = tail $ concatMap (replicate position) basePattern

  basePattern :: [Int]
  basePattern = cycle [0, 1, 0, -1]

lastDigit :: Int -> Int
lastDigit = (`mod` 10) . abs

iterationN :: Int -> (a -> a) -> (a -> a)
iterationN n f = (!! max 0 n) . iterate' f


solvePt1 :: [Int] -> String
solvePt1 = display . take 8 . iterationN 100 fft

solvePt2 :: String -> String
solvePt2 input =
  display
    . take 8
    . iterationN 100 comp
    . drop offset
    . repList 10000
    . parse
    $ input
 where
  offset = read . take 7 $ input
  comp   = map lastDigit . reverse . cumSum . reverse
  cumSum = tail . scanl' (+) 0


-- A more efficient list replication concat.replicate n implementation for large n?
-- BY RHIND PAPYRUS 'EGYPTIAN' OR 'ETHIOPIAN' MULTIPLICATION ------------------
repList :: Int -> [a] -> [a]
repList n s =
  foldr
      (\(d, x) a -> if d > 0 -- Is this power of 2 needed for the binary recomposition ?
        then mappend a x
        else a
      )
      mempty
    $ zip
        (unfoldr
          (\h -> if h > 0
            then Just $ swap (quotRem h 2) -- Binary decomposition of n
            else Nothing
          )
          n
        )
        (iterate (join mappend) s) -- Iterative duplication ( mappend to self )



main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = parse input
  putStr "Part 1: "
  print $ solvePt1 pinput
  putStr "Part 2: "
  let offset = (read . take 7 $ input) :: Int
  when (offset <= length pinput `div` 2) (error "offset too small")
  print $ solvePt2 input


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
  describe "Part 2" $ do
    it "Example 4" $ do
      solvePt2 "03036732577212944063491565474664" `shouldBe` "84462026"
    it "Example 5" $ do
      solvePt2 "02935109699940807407585447034323" `shouldBe` "78725270"
    it "Example 6" $ do
      solvePt2 "03081770884921959731165446850517" `shouldBe` "53553731"


