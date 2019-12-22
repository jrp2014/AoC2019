{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Day22 where

import           Data.Semigroup                 ( stimes )
import           GHC.TypeNats                   ( KnownNat )
import           Math.NumberTheory.Moduli.Class ( Mod
                                                , getNatVal
                                                )


-- Based on glguy's, mstksg's  and naim42's solutions

data Command
  = Cut     Integer
  | Increment Integer
  | Reverse
  deriving Show

parseCommands :: String -> [Command]
parseCommands = map parseLine . lines

parseLine :: String -> Command
parseLine s = case words s of
  ["deal", "into", "new", "stack"] -> Reverse
  ["deal", "with", "increment", n] -> Increment (read n)
  ["cut", n] -> Cut (read n)
  _          -> error $ "Bad input:" ++ s


------------------------------------------------------------------------
-- Linear shuffles
------------------------------------------------------------------------

-- `a :+ b` represents the linear polynomial `aX + b`.
data Linear a = a :+ a

-- Linear polynomials form a monoid under left-to-right composition.
instance Num a => Semigroup (Linear a) where
  (a :+ b) <> (c :+ d) = (c * a) :+ (c * b + d) -- c(aX + b) + d = caX + cb + d

instance Num a => Monoid (Linear a) where
  mempty = 1 :+ 0

-- Evaluate `ax + b = ?`
($@) :: Num a => Linear a -> a -> a
(a :+ b) $@ x = a * x + b

-- Solve `a? + b = y`
($?) :: Fractional a => Linear a -> a -> a
(a :+ b) $? y = (y - b) / a


-- Represents a shuffling technique as an affine transformation (linear polynomial)
-- of a cards position, modulo `n`.
toLinear :: KnownNat n => Command -> Linear (Mod n)
toLinear Reverse       = (-1) :+ (-1)
toLinear (Cut       k) = 1 :+ fromInteger (-k)
toLinear (Increment k) = fromInteger k :+ 0


main :: IO ()
main = do
  input <- readFile "input.txt"
  let commands = parseCommands input

  let shuffle :: KnownNat n => Linear (Mod n)
      shuffle = foldMap toLinear commands

  putStr "Part 1: "
  print . getNatVal $ shuffle @10007 $@ 2019

  putStr "Part 2: "
  print . getNatVal $ 101741582076661 `stimes` shuffle @119315717514047 $? 2020


