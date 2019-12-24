module Day23 where

import           Control.Applicative            ( liftA
                                                , liftA2
                                                )
import           Data.Sequence                 as S

import           IntCode

parse :: String -> Memory
parse s = S.fromList . read $ '[' : s ++ "]"


main :: IO ()
main = do
  inp <- readFile "input.txt"
  let pinp = parse inp
--  print $ animate pinp
  print "LO"

data Packet = Packet Int Int Int
type Node = [Int] -> [Int]
type Network = [Node]


boot :: Memory -> Int -> Node
boot prog n inputs = execute prog (n : -1 : inputs)

botNet :: Memory -> Network
botNet prog = [ boot prog n | n <- [0 .. 49] ]


step :: Memory -> [Int] -> [[Int]] -> [[Int]]
step prog inputs = liftA2 (boot prog) inputs
