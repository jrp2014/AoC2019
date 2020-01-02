{-# LANGUAGE RecordWildCards #-}
module Day23 where

import           Control.Arrow                  ( first )
import qualified Data.Map                      as M
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as S

import           Data.List                      ( unfoldr )

import           IntCode

parse :: String -> Memory
parse s = Seq.fromList . read $ '[' : s ++ "]"


type Payload = (Int, Int)
data Network = Network { computers :: M.Map Int Machine, nat :: Maybe Payload } deriving Show


getPayloads :: Machine -> ([(Int, [Int])], Machine)
getPayloads m = case output (run m) of
  (a : x : y : rest) -> ([(a, [x, y])], m { output = rest })
  []                 -> ([], m)

step :: Network -> (Maybe Payload, Network)
step Network {..} =
  (if isIdle then nat' else Nothing, Network computers'' nat')
 where
  packets :: M.Map Int [Int]
  computers' :: M.Map Int Machine
  (packets, computers') =
    first (M.fromListWith (++)) $ traverse getPayloads computers

  nat' :: Maybe Payload
  nat' | Just (x : y : _) <- packets M.!? 255 = Just (x, y)
       | otherwise                            = nat

  isIdle :: Bool
  isIdle = M.null packets

  distributePayloads :: Int -> Machine -> Machine
  distributePayloads 0 m | isIdle, Just (x, y) <- nat' =
    run $ m { input = input m ++ [x, y] }
  distributePayloads a m =
    run $ m { input = input m ++ M.findWithDefault [-1] a packets}

  computers'' :: M.Map Int Machine
  computers'' = M.mapWithKey distributePayloads computers'


solvePt1 :: String -> Int
solvePt1 s = head nats
 where
  network = Network
    (M.fromList [ (i, mkMachine (parse s) [i]) | i <- [0 .. 49] ])
    Nothing
  nats = [ y | Just (_, y) <- unfoldr (Just . step) network ]

solvePt2 :: String -> Int
solvePt2 s = firstDuplicate nats
 where
  network = Network
    (M.fromList [ (i, mkMachine (parse s) [i]) | i <- [0 .. 49] ])
    Nothing
  nats = [ y | Just (_, y) <- unfoldr (Just . step) network ]

findDuplicates :: Ord a => [a] -> [a]
findDuplicates = go S.empty where
  go seen (x : xs) | x `S.member` seen = x : xs'
                   | otherwise         = xs'
    where xs' = go (S.insert x seen) xs
  go _ [] = []

firstDuplicate :: Ord a => [a] -> a
firstDuplicate = head . findDuplicates

main :: IO ()
main = do
  inp <- readFile "input.txt"
  putStr "Part 1: "
  print $ solvePt1 inp
  putStr "Part 2: "
  print $ solvePt2 inp
