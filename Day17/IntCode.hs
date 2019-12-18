module IntCode where

import           Data.Bool                      ( bool )
import qualified Data.Sequence                 as S

type Memory = S.Seq Int

data Machine
  = Machine
      { pc :: Int, -- program counter
        memory :: Memory,
        input :: [Int],
        relBase :: Int
      }

mkMachine :: [Int] -> Memory -> Machine
mkMachine inp mem = Machine { pc      = 0
                            , memory  = mem S.>< S.replicate 4096 0
                            , input   = inp
                            , relBase = 0
                            }

run :: Machine -> [Int]
run m@(Machine counter mem inp relB) = case opcode of
  -- (+)
  1 -> run m { pc = counter + 4, memory = setP 3 (getP 1 + getP 2) mem }

  -- (*)
  2 -> run m { pc = counter + 4, memory = setP 3 (getP 1 * getP 2) mem }

  -- input
  3 -> if null inp
    then []
    else run m { pc     = counter + 2
               , memory = setP 1 (head inp) mem
               , input  = tail inp
               }

  -- output
  4 -> -- produces output in reverse order, which seems to give the right
       -- laziness characteristics for Day 13
    getP 1 : run m { pc = counter + 2 }

  -- Jump if true
  5 -> run m { pc = bool (getP 2) (counter + 3) (getP 1 == 0) }

  -- Jump if false
  6 -> run m { pc = bool (getP 2) (counter + 3) (getP 1 /= 0) }

  -- less than
  7 ->
    run m { pc = counter + 4, memory = setP 3 (bool 0 1 (getP 1 < getP 2)) mem }

  -- equals
  8 -> run m { pc     = counter + 4
             , memory = setP 3 (bool 0 1 (getP 1 == getP 2)) mem
             }

  -- adjust relative base
  9  -> run m { pc = counter + 2, relBase = relB + getP 1 }

  -- halt
  99 -> []

  o  -> error ("Invalid opcode " ++ show o ++ " at " ++ show counter)
 where
  get :: Int -> Int
  get = S.index mem -- get directly

  -- set, lazily
  setP :: Int -> Int -> Memory -> Memory
  -- setP position value memry = value `seq` S.update (param position) value memry
  setP position value memry = case mode position of
    0 -> S.update (param position) value memry
    1 -> error "Cannot set directly"
    2 -> S.update (relB + param position) value memry
    p ->
      error
        $  "Invalid setP parameter mode "
        ++ show p
        ++ " at pc "
        ++ show counter

  -- The value of the ith parameter, where the 0th parameter is the opcode
  param :: Int -> Int
  param i = get (counter + i)

  opcode :: Int
  opcode = param 0 `mod` 100

  getP position = case mode position of
    0 -> get (param position) -- indirect
    1 -> param position -- direct
    2 -> get (relB + param position) -- relative
    p ->
      error
        $  "Invalid getP parameter mode "
        ++ show p
        ++ " at pc "
        ++ show counter

  -- the mode of the positionth parameter
  -- 0 = indirect / position mode
  -- 1 = immediate mode
  -- 2 = relative mode
  mode :: Int -> Int
  mode position = digit (position + 1) (param 0)
   where
    -- the value of the positionth digit of n
    digit :: Int -> Int -> Int
    digit pos n = n `div` (10 ^ pos) `mod` 10

execute :: Memory -> [Int] -> [Int]
execute mem inputs = run $ mkMachine inputs mem


