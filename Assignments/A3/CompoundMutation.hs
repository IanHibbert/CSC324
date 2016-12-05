{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer(..), findKey,
    Value(..),
    StateOp(..),
    alloc, find_space,
    free,
    (>>>), (>~>),
    returnVal, runOp)
    where

import AList (AList, lookupA, insertA, updateA, deleteA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer | PP Integer Integer deriving Show

-- A type representing operation on Memory state
data StateOp a = StateOp (Memory -> (a, Memory))

-- A type representing a person with two attributes :
-- age and whether they are a student or not.
data Person = Person Integer Bool deriving Show


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
                    newStateOp = op2 x
                in
                  runOp newStateOp mem1
              )
--return
returnVal :: a -> StateOp a
returnVal a = StateOp(\mem ->
                      (a, mem))

-- Allocate
alloc :: Mutable a => a -> StateOp (Pointer a)
alloc val = StateOp(\mem ->
                    runOp (def (find_space mem 0) val) mem)

find_space :: Memory -> Integer -> Integer
find_space mem acc = if (findKey mem acc)
                      then
                        find_space mem (acc + 1)
                      else
                        acc


free :: Mutable a => Pointer a -> StateOp ()
free (P a) = StateOp (\mem ->
                    if (findKey mem a)
                      then
                        ((), deleteA mem a)
                      else
                        ((), mem)
                  )


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

instance Mutable Person where
  get (PP a b) =  get (P a) >~> \age ->
                  get (P b) >~> \student ->
                  returnVal (Person age student)


  set (PP a b) (Person age student) = set (P a) age >>> set (P b) student

  def ptr (Person age student) = def ptr age >~>
                                \age_ptr -> alloc student >~>
                                \student_ptr -> let (P a) = age_ptr
                                                    (P b) = student_ptr
                                                in
                                returnVal(PP a b)



(@@) :: Pointer a -> Bool -> Pointer b
(PP age student) @@  attr = if (attr == True) then
                            (P age)
                            else if (attr == False) then
                            (P student)
                            else
                              error "attribute not found"

age :: Bool
age = True

isStudent :: Bool
isStudent = False


personTest :: Person -> Integer -> StateOp (Integer, Bool , Person)
personTest person x =
  def 1 person >~> \personPointer ->
  get (personPointer @@ age) >~> \oldAge ->
  set (personPointer @@ age) x >>>
  get (personPointer @@ isStudent) >~> \stu ->
  get (personPointer @@ age) >~> \newAge ->
  set personPointer (Person (2 * newAge) (not stu)) >>>
  get personPointer >~> \newPerson ->
  get (personPointer @@ isStudent) >~> \newStu ->
  returnVal (oldAge ,newStu, newPerson)
