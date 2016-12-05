{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

module MutationUser(pointerTest, swap, swapCycle) where

import Mutation (
    get, set, def, returnVal, (>~>), (>>>),
    Mutable, Pointer, Memory, StateOp(..), returnVal, runOp
    )


-- PART 3: Calling with references

swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap p1 p2 = (get p1) >~>
              \p1_val -> (get p2) >~>
              \p2_val -> (set p2 p1_val) >>>
              (set p1 p2_val) >>>
              returnVal ()

swapCycle :: Mutable a => [Pointer a] -> StateOp ()
-- swapCycle [] = returnVal ()
-- swapCycle (x:[]) = returnVal()
-- swapCycle (x:y:rest) = swap x y >>> swapCycle(y:rest)
swapCycle lst = case lst of
                [] -> returnVal()
                [x] -> returnVal()
                (x:xs:rest) -> swap x xs >>> swapCycle(xs:rest)

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
-- pointerTest :: Integer -> Memory -> ((Pointer Integer, Pointer Bool), Memory)
-- pointerTest n mem = let (a, mem1) = runOp (def 100 (n + 3)) mem
--                         (x, y) = runOp (def 500 (n > 0)) mem1
--                         in
--                         ((a, x), y)

pointerTest :: Integer -> StateOp (Pointer Integer, Pointer Bool)
pointerTest n  = StateOp (\mem ->
                          let (a, mem1) = runOp (def 100 (n + 3)) mem
                              (x, y) = runOp (def 500 (n > 0)) mem1
                              in
                              ((a, x), y))
