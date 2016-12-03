{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer(..), findKey,
    Value(..),
    StateOp(..))
    where

import AList (AList, lookupA, insertA, updateA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer deriving Show

-- A type representing operation on Memory state
data StateOp a = StateOp (Memory -> (a, Memory))

findKey :: Memory -> Integer -> Bool
findKey [] val = False
findKey mem val = let (a, b) = head mem
                    in
                    if (a == val)
                      then
                        True
                      else
                        findKey (tail mem) val

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

-- Then
(>>>) :: StateOp a -> StateOp b -> StateOp b
op1 >>> op2 = StateOp (\m ->
  let (_, m1) = runOp op1 m
  in runOp op2 m1)

-- Bind
(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
op1 >~> op2 = StateOp (\mem ->
                let (x, mem1) = runOp op1 mem
                    mem2 = op2 x
                in
                  runOp mem2 mem1
              )
--return
returnVal :: a -> StateOp a
returnVal a = StateOp(\mem ->
                      (a, mem))

-- Allocate
alloc :: Mutable a => a -> StateOp (Pointer a)


-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: Pointer a -> StateOp a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Pointer a -> a -> StateOp ()

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Integer -> a -> StateOp (Pointer a)

instance Mutable Integer where
  get (P ptr) = StateOp (\mem ->
                    if findKey mem ptr
                    then
                      case lookupA mem ptr of
                        IntVal a -> (a, mem)
                    else
                      error "key not found"
                      )

  set (P ptr) val = StateOp (\mem -> ((),
                    if (findKey mem ptr)
                    then
                      updateA mem (ptr, IntVal val)
                    else
                      error "key not found"
                      ))

  def ptr val = StateOp (\mem-> (
                    if findKey mem ptr
                    then
                      error "location taken go away"
                    else
                      ((P ptr), insertA mem (ptr, IntVal val))
                      ))



instance Mutable Bool where
  get (P ptr) = StateOp (\mem ->
                    if findKey mem ptr
                    then
                      case lookupA mem ptr of
                        BoolVal a -> (a, mem)
                    else
                      error "key not found"
                      )

  set (P ptr) val = StateOp (\mem -> ((),
                    if (findKey mem ptr)
                    then
                      updateA mem (ptr, BoolVal val)
                    else
                      error "key not found"
                      ))

  def ptr val = StateOp (\mem-> (
                    if findKey mem ptr
                    then
                      error "location taken go away"
                    else
                      ((P ptr), insertA mem (ptr, BoolVal val))
                      ))


testMem :: Memory
testMem = [(1, IntVal 10), (2, IntVal 30), (3, BoolVal True), (4, BoolVal False)]

p1 :: Pointer Integer
p1 = P 1

p3 :: Pointer Bool
p3 = P 3


f :: Integer -> StateOp Bool
f x =
   def 1 4 >~> \p1 ->
   def 2 True >~> \p2 ->
   set p1 (x + 5) >>>
   get p1 >~> \y ->
   set p2 (y > 3) >>>
   get p2

g :: Integer -> StateOp Integer
g x =
  def 1 (x + 4) >~> \p ->
  get p >~> \y ->
  returnVal (x * y)
