module Day04 where

import Data.List (group)

lb :: Integer
lb = 136818

ub :: Integer
ub = 685979

range :: [Integer]
range = [lb .. ub]

-- pairs of adjacent elements are monotinically non-decreasing?
monotonic :: Ord a => [a] -> Bool
monotonic xs = and (zipWith (<=) xs (tail xs))

numofequaladjacents :: Eq a => [a] -> [Int]
numofequaladjacents = map length . group

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

main :: IO ()
main
  -- lb and ub are 6-digit
 = do
  let adjacents =
        map numofequaladjacents $ filter monotonic $ map show [lb .. ub]
  putStr "Part 1 "
  print (count (any (> 1)) adjacents) -- at least one equal adjacent digit
  putStr "Part 2 "
  print (count (elem 2) adjacents) -- 2 adjacent digits
