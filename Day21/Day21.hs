module Day19 where

import           Data.Sequence                 as S
import           Data.Char                      ( ord )
import           IntCode

parse :: String -> Memory
parse s = S.fromList . read $ '[' : s ++ "]"

solve :: Memory -> [String] -> Int
solve prog ainput = last $ execute prog (ord <$> unlines ainput)

input1 :: [String]
input1 =
  [ "OR A J"
  ,  -- J = A
    "AND B J"
  , -- J = A & B
    "AND C J"
  , -- J = A & B & C
    "NOT J J"
  , -- J = !(A & B & C)
    "AND D J"
  , -- J = !(A & B & C) & D
    "WALK"
  ]

input2 :: [String]
input2 =
  [ "OR A J"
  ,  -- J = A
    "AND B J"
  , -- J = A & B
    "AND C J"
  , -- J = A & B & C
    "NOT J J"
  , -- J = !(A & B & C)
    "AND D J"
  , -- J = !(A & B & C) & D
    "OR E T"
  ,  -- T = E
    "OR H T"
  ,  -- T = E | H
    "AND T J"
  , -- J = !(A & B & C) & D & (E | H)
    "RUN"
  ]

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let pinput = parse inp

  putStr "Part 1: "
  print $ solve pinput input1

  putStr "Part 2: "
  print $ solve pinput input2
