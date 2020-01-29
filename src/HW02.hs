module HW02 where

{-

Name: 

<put your name here>

Collaboration Statement:

<put your collaboration statement here>

Collaboration on high-level ideas and approach on assignments is
encouraged. Copying someone else's work is not allowed. Copying solutions
from an online source is not allowed. Any collaboration or online
resources, even if used only a small amount, must be declared in your
assignment when you submit it. For example: “I discussed high-level
strategies for solving problem 2 and 5 with Alex; I found this
stackoverflow post (<link>) helpful while working on problem 3.” Students
caught copying work are eligible for immediate failure of the course and
disciplinary action by the University. All academic integrity misconduct
will be treated according to UVM's Code of Academic Integrity.

-}

import Util.Testing

import Lang.Trees

-- ========== --
-- SUBMITTING --
-- ========== --

-- How to submit:
--
-- Submit a copy of this file only “HWXX.hs” (for XX = 01, 02, etc.) to
-- gradescope.

-- ========== --
-- ASSIGNMENT --
-- ========== --

-- For your reference, here are three functions written in Haskell:
-- `factorial`, `dropCommonPrefix` and `swapFirstTwoChars`. Examples are
-- written with [XN] in front for “example N”, and homework problems are marked
-- with [EN] in front for “exercise N”. Your job is to fill in all function
-- definitions which currently are `error "TODO"`.

----------
-- [X1] --
----------

-- Compute the sum of all elements in a tree.
--
-- See test cases in tests/hw02/x1/.

treeSum :: Tree -> Int
treeSum t = case t of
  Leaf -> 0
  Node i t1 t2 -> i + treeSum t1 + treeSum t2

testTreeSum :: Test
testTreeSum = TestDir
  ( "X1"
  , "treeSum"
  , treeSum
  , "tests/hw02/x1"
  , parseRTest
  )

----------
-- [X2] --
----------

-- Return a copy of the input tree with all node values incremented by one.
--
-- See test cases in tests/hw02/x2/.

treeIncrement :: Tree -> Tree
treeIncrement t = case t of
  Leaf -> Leaf
  Node i t1 t2 -> Node (i+1) (treeIncrement t1) (treeIncrement t2)

testTreeIncrement :: Test
testTreeIncrement = TestDir
  ( "X2"
  , "treeIncrement"
  , treeIncrement
  , "tests/hw02/x2"
  , parseTTest
  )

---------------
-- [E1]: ★☆☆ --
---------------

-- Compute the product of all elements in a tree
--
-- See test cases in tests/hw02/e1/.

treeProduct :: Tree -> Int
treeProduct t = error "TODO"

testTreeProduct :: Test
testTreeProduct = TestDir
  ( "E1"
  , "treeProduct"
  , treeProduct
  , "tests/hw02/e1"
  , parseRTest
  )

---------------
-- [E2]: ★★☆ --
---------------

-- Compute the height of the tree. Nodes are height 1 and leaves are height 0.
--
-- You may want to use the `max` function:
--
--     max :: Int -> Int -> Int
--
-- e.g., max 2 3 == max 3 2 == 3
--
-- See test cases in tests/hw02/e3/.

treeHeight :: Tree -> Int
treeHeight t = error "TODO"

testTreeHeight :: Test
testTreeHeight = TestDir
  ( "E2"
  , "treeHeight"
  , treeHeight
  , "tests/hw02/e2"
  , parseRTest
  )

---------------
-- [E3]: ★★★ --
---------------

-- Compute a copy of the input tree where all left and right branches are
-- swapped with each other recursively, resulting in a sequentially reversed
-- tree.
--
-- See test cases in tests/hw02/e2/.

treeReverse :: Tree -> Tree
treeReverse t = error "TODO"

testTreeReverse :: Test
testTreeReverse = TestDir
  ( "E3"
  , "treeReverse"
  , treeReverse
  , "tests/hw02/e3"
  , parseTTest
  )

main :: IO ()
main = runTests 
  [ testTreeSum
  , testTreeIncrement
  , testTreeProduct
  , testTreeHeight
  , testTreeReverse
  ]
