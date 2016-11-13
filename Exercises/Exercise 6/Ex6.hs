{- Exercise 6, due (Nov 12, 11:50pm)

General exercise instructions:
- Exercises must be done *individually*.
- You may not import any Haskell libraries, unless explicitly told to.
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!

This exercise is mainly a chance to practice writing code in Haskell,
although we'll take advantage of lazy evaluation to create some nice
infinite data structures. :)
-}

-- This line creates a module to allow exporting of functions.
-- DON'T CHANGE IT!
module Ex6 (primes, Tree(Empty, Node), treeHeight) where

-- Question 1
-- Define 'primes', an infinite list of primes (in ascending order).
-- Remember that 1 is not a prime, but 2 is a prime.
-- You may want to use the definition of 'nats' from the lecture

nats = 2 : map (\x -> x + 1) nats

factors :: Integer -> [Integer]
factors n = [i | i <-[1..n], mod n i == 0]

is_prime :: Integer -> Bool
is_prime n = (length (factors n)) == 2

primes :: [Integer]
primes = filter is_prime nats



-- This is a datatype to define a binary tree. It follows the
-- recursive definition of a binary tree, which is:
--   - an empty tree
--   - a node containing a value with a left and right subtree
-- For this exercise, we'll stick with binary trees of integers.
-- In the second constructor, the first 'Tree' represents the left
-- subtree, and the second represents the right subtree.
data Tree = Empty | Node Integer Tree Tree deriving Show

-- An example of a tree.
tree :: Tree
tree = Node 5 (Node 3 Empty
                      (Node 3 Empty Empty))
              (Node 5 (Node 1 Empty Empty)
                      (Node (-10) Empty Empty))

-- Question 2
-- Define 'treeHeight', which takes a tree and returns the height of a tree.
-- Note that the empty tree has a height of 0.
treeHeight :: Tree -> Integer
treeHeight Empty = 0
treeHeight (Node a left Empty) = 1 + treeHeight left
treeHeight (Node a Empty right) = 1 + treeHeight right
treeHeight (Node a left right) = 1 + (max (treeHeight left) (treeHeight right))
