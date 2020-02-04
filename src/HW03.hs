module HW03 where

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

import Data.Set (Set)
import qualified Data.Set as Set

import Util.Testing

import Util.Lex

import qualified Lang.L1 as L1
import qualified Lang.L1M as L1M
import qualified Lang.L1MN as L1MN
import qualified Lang.L2 as L2
import qualified Lang.L2C as L2C

-- ========== --
-- SUBMITTING --
-- ========== --

-- How to submit:
--
-- Submit a copy of this file only “HWXX.hs” (for XX = 01, 02, etc.) to
-- gradescope.

-- ======== --
-- EXAMPLES --
-- ======== --

----------
-- [X1] --
----------

-- Here is an interpreter for L1:

interpL1 :: L1.Expr -> L1.Answer
interpL1 e = case e of
  L1.IntE i -> L1.ValueA (L1.IntV i)
  L1.PlusE e1 e2 -> case (interpL1 e1, interpL1 e2) of
    (L1.ValueA (L1.IntV i1), L1.ValueA (L1.IntV i2)) -> L1.ValueA (L1.IntV (i1 + i2))
  L1.TimesE e1 e2 -> case (interpL1 e1, interpL1 e2) of
    (L1.ValueA (L1.IntV i1), L1.ValueA (L1.IntV i2)) -> L1.ValueA (L1.IntV (i1 * i2))

testInterpL1 :: Test
testInterpL1 = TestDir
  ( "X1"
  , "interpL1"
  , interpL1
  , "tests/hw03/x1"
  , parseTest L1.pExpr L1.pAnswer
  )

----------
-- [X2] -- 
----------

-- Here is a desugaring pass for minus into L1:

desugarMinus :: L1M.Expr -> L1.Expr
desugarMinus e = case e of
  L1M.IntE i -> L1.IntE i
  L1M.PlusE e1 e2 -> L1.PlusE (desugarMinus e1) (desugarMinus e2)
  L1M.TimesE e1 e2 -> L1.TimesE (desugarMinus e1) (desugarMinus e2)
  L1M.MinusE e1 e2 -> L1.PlusE (desugarMinus e1) (L1.TimesE (L1.IntE (-1)) (desugarMinus e2))

testDesugarMinus :: Test
testDesugarMinus = TestDir
  ( "X2"
  , "desugarMinus"
  , desugarMinus
  , "tests/hw03/x2"
  , parseTest L1M.pExpr L1.pExpr
  )

----------
-- [X3] --
----------

-- Here is a desugaring pass for minus and negation into L1:

desugarMinusNeg :: L1MN.Expr -> L1.Expr
desugarMinusNeg e = case e of
  L1MN.IntE i -> L1.IntE i
  L1MN.PlusE e1 e2 -> L1.PlusE (desugarMinusNeg e1) (desugarMinusNeg e2)
  L1MN.TimesE e1 e2 -> L1.TimesE (desugarMinusNeg e1) (desugarMinusNeg e2)
  L1MN.MinusE e1 e2 -> L1.PlusE (desugarMinusNeg e1) (L1.TimesE (L1.IntE (-1)) (desugarMinusNeg e2))
  L1MN.NegE e' -> L1.TimesE (L1.IntE (-1)) (desugarMinusNeg e')
  -- -- also works
  -- L1MN.NegE e' -> desugarMinusNeg (L1MN.MinusE (L1MN.IntE 0) e')
  -- -- doesnt work (because L1.MinusE isn't valid)
  -- L1MN.NegE e' -> L1.MinusE (L1.IntE 0) (desugarMinusNeg e')

testDesugarMinusNeg :: Test
testDesugarMinusNeg = TestDir
  ( "X3"
  , "desugarMinusNeg"
  , desugarMinusNeg
  , "tests/hw03/x3"
  , parseTest L1MN.pExpr L1.pExpr
  )

-- ========== --
-- ASSIGNMENT --
-- ========== --

---------------
-- [E1]: ★★★ --
---------------

-- Write an interpreter for the L2 langauge which includes booleans, boolean
-- operations, and equality. The equality comparison operation should work on
-- both integer and boolean values.
--
-- HINTS:
-- - the operators for "equality", "logical or", and "logical and" in Haskell are
--   `==`, `||` and `&&` respectively. The function for logical negation in
--   Haskell is `not`.

interpL2 :: L2.Expr -> L2.Answer
interpL2 e = error "TODO"

testInterpL2 :: Test
testInterpL2 = TestDir
  ( "E1"
  , "interpL2"
  , interpL2
  , "tests/hw03/e1"
  , parseTest L2.pExpr L2.pAnswer
  )

---------------
-- [E2]: ★★☆ --
---------------

-- Write a desugaring pass from L2 to L2C which doesn't have "logical and". You
-- should desugar `e1 && e2` to `!(!e1 || !e2)`.

desugarL2 :: L2.Expr -> L2C.Expr
desugarL2 e = error "TODO"

testDesugarL2 :: Test
testDesugarL2 = TestDir
  ( "E2"
  , "desugarL2"
  , desugarL2
  , "tests/hw03/e2"
  , parseTest L2.pExpr L2C.pExpr
  )

---------------
-- [E3]: ★☆☆ --
---------------

-- Return true if the two sets share a common element, and false otherwise.
-- Compute this by checking if the size of their intersection is strictly
-- greater than 0.
--
-- HINTS:
--     
--     Set.member :: Ord a => a -> Set a -> Bool
--     Set.empty :: Set a
--     Set.union :: Ord a => Set a -> Set a -> Set a
--     Set.intersection :: Ord a => Set a -> Set a -> Set a
--     Set.difference :: Ord a => Set a -> Set a -> Set a
--     Set.size :: Set a -> Int
--     Set.insert :: Ord a => a -> Set a -> Set a
--     Set.delete :: Ord a => a -> Set a -> Set a
--     Set.fromList :: Ord a => [a] -> Set a
--     Set.toList :: Set a -> [a]
--
-- (also see official documentation: http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Set.html)

overlapping :: Set Integer -> Set Integer -> Bool
overlapping xs ys = error "TODO"
    
testOverlapping :: Test
testOverlapping = TestDir
  ( "E3"
  , "overlapping"
  , uncurry overlapping
  , "tests/hw03/e3"
  , parseTest (pPair (pSet pInt) (pSet pInt))  pBool
  )

main :: IO ()
main = runTests 
  -- examples
  [ testInterpL1
  , testDesugarMinus
  , testDesugarMinusNeg
  -- assignment
  , testInterpL2
  , testDesugarL2
  , testOverlapping
  ]
