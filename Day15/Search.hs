{-|
Module      : Main
Description : Generalized search functions
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Search where

import qualified Queue  as Queue
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

{-# INLINE dfs #-}
dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs = dfsOn id

dfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
dfsOn rep next start = loop Set.empty [start]
  where
    loop _ [] = []
    loop seen (x:xs)
      | Set.member r seen =     loop seen xs
      | otherwise         = x : loop seen1 (next x ++ xs)
      where
        r     = rep x
        seen1 = Set.insert r seen

{-# INLINE bfs #-}
bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id

-- | Enumerate the reachable states in breadth-first order
-- given a successor state function and initial state.
--
-- States are compared for equality using the representative
-- function. If the representatives are equal the state is
-- considered already visited.
{-# INLINE [0] bfsOn #-}
bfsOn ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  a          {- ^ initial state             -} ->
  [a]        {- ^ reachable states          -}
bfsOn rep next start = bfsOnN rep next [start]

{-# INLINE [0] bfsOnN #-}
bfsOnN ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  [a]         {- ^ initial state             -} ->
  [a]        {- ^ reachable states          -}
bfsOnN rep next start = loop Set.empty (Queue.fromList start)
  where
    loop _ Queue.Empty = []
    loop seen (x Queue.:<| q1)
      | Set.member r seen =     loop seen  q1
      | otherwise         = x : loop seen1 q2
      where
        r     = rep x
        seen1 = Set.insert r seen
        q2    = Queue.appendList (next x) q1

{-# RULES "bfsOn/Int" bfsOn = bfsOnInt #-}
{-# INLINE bfsOnInt #-}
bfsOnInt :: (a -> Int) -> (a -> [a]) -> a -> [a]
bfsOnInt rep next start = loop IntSet.empty (Queue.singleton start)
  where
    loop seen q =
      case q of
        Queue.Empty -> []
        x Queue.:<| q1
          | IntSet.member r seen ->     loop seen  q1
          | otherwise            -> x : loop seen1 q2
          where
            r     = rep x
            seen1 = IntSet.insert r seen
            q2    = Queue.appendList (next x) q1
