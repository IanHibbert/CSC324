{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

module MutationUser (
    swap,
    -- pointerTest,
    swapCycle)

    where

import Mutation (
    get, set, def, returnVal, (>~>), (>>>),
    Mutable, Pointer, Memory, StateOp(..)
    )


-- PART 3: Calling with references

swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap p1 p2 = (get p1) >~>
              \p1_val -> (get p2) >~>
              \p2_val -> (set p2 p1_val) >>>
              (set p1 p2_val) >>>
              returnVal ()

swapCycle :: Mutable a => [Pointer a] -> StateOp ()
swapCycle [] = returnVal ()
swapCycle (x:[]) = returnVal()
swapCycle (x:xs:rest) = swap x xs >>> swapCycle(xs:rest)
  -- let last_ptr = head lst
  --                   alt_lst = (drop 1 lst) ++ [last_ptr]
  --              in
  --              zip swap lst alt_lst


-- swap p1 p2 = StateOp ((),
--                       let (x, mem1) = runOp (get p1)
--                           (y, mem2) = runOp (get p2)
--                           in
--                           runOp (set p1) y
--                           runOp (set p2) x)


-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
-- pointerTest :: Integer -> Memory -> ((Pointer Integer, Pointer Bool), Memory)
-- pointerTest n mem = let int = (n + 3)
--                         boo = (n > 0)
--                     in
--                     let m1 = (def mem 100 int)
--                     in
--                     (def m1 500 boo)
