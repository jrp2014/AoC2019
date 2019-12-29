module Day25 where

import qualified Data.Sequence                 as S
import           Data.Char                      ( ord
                                                , chr
                                                )
import           Data.List                      ( isInfixOf )

import           IntCode

parse :: String -> Memory
parse s = S.fromList . read $ '[' : s ++ "]"


explore :: Memory -> String -> String
explore prog = map chr . execute prog . map ord

play :: Memory -> [String] -> String
play prog inputs = process $ explore prog $ unlines inputs
 where
  process s
    | "Alert! Droids on this ship are heavier than the detected value"
      `isInfixOf` s
    = "-"
    | -- too light
      "Alert! Droids on this ship are lighter than the detected value"
      `isInfixOf` s
    = "+"
    | -- too heavy
      otherwise
    = s


main :: IO ()
main = do
  code <- readFile "input.txt"
  let pcode = parse code
  --interact $ explore pcode
  -- putStr $ play pcode getToCheckPoint
  mapM_ (putStr . play pcode) brute
  print "Done"


getToCheckPoint :: [String]
getToCheckPoint =
  [ east
  , take_ manifold
  , south
  , take_ peas
  , north
  , west
  , south
  , take_ heater
  , south
  , take_ matter
  , north
  , east
  , north
  , west
  , south
  , take_ antenna
  , north
  , east
  , south
  , east
  , take_ rice
  , north
  , take_ bottle
  , north
  , take_ cat6
  , west
  , inv
  ]

antenna, bottle, cat6, east, heater, inv, manifold, matter, north, peas, rice, south, west
  :: String
north = "north"
south = "south"
east = "east"
west = "west"
inv = "inv"

heater = "space heater"
antenna = "antenna"
peas = "whirled peas"
manifold = "manifold"
matter = "dark matter"
cat6 = "spool of cat6"
rice = "bowl of rice"
bottle = "klein bottle"


take_, drop_ :: String -> String
take_ x = "take " ++ x
drop_ x = "drop " ++ x

objects :: [String]
objects = [heater, antenna, peas, manifold, matter, cat6, rice, bottle]

-- brute is v dumb; it doesn't even add of subtract from the current inventory, but starts
-- from scratch each time
brute :: [[String]]
brute =
  map
      (\s -> getToCheckPoint ++ map drop_ objects ++ map take_ s ++ [inv, north]
      )
    $ powerset objects

powerset :: [a] -> [[a]]
powerset = foldr (\x acc -> acc ++ map (x :) acc) [[]]

