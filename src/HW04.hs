module HW04 where

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

import qualified Data.Map as Map

import Util.Testing

import Util.Lex

import qualified Lang.L3 as L3

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

-- Here is a well-formedness checker for L3.

wf :: Set String -> L3.Expr -> Bool
wf scope e = case e of
  L3.IntE _i -> True
  L3.PlusE e1 e2 -> wf scope e1 && wf scope e2
  L3.TimesE e1 e2 -> wf scope e1 && wf scope e2
  L3.BoolE _b -> True
  L3.IfE e1 e2 e3 -> wf scope e1 && wf scope e2 && wf scope e3
  L3.VarE x -> Set.member x scope
  L3.LetE x e1 e2 -> wf scope e1 && wf (Set.insert x scope) e2

testX1 :: Test
testX1 = TestDir
  ( "X1"
  , "wf"
  , uncurry wf
  , "tests/hw04/x1"
  , parseTest (pPair (pSet pVar) L3.pExpr) pBool
  )

----------
-- [X2] -- 
----------

-- Here is a call-by-value interpreter for L3 that uses environments.

interpEnv :: L3.Env -> L3.Expr -> L3.Answer
interpEnv env e = case e of
  L3.IntE i -> L3.ValueA (L3.IntV i)
  L3.PlusE e1 e2 -> case (interpEnv env e1, interpEnv env e2) of
    (L3.ValueA (L3.IntV i1), L3.ValueA (L3.IntV i2)) -> L3.ValueA (L3.IntV (i1 + i2))
    _ -> L3.BadA
  L3.TimesE e1 e2 -> case (interpEnv env e1, interpEnv env e2) of
    (L3.ValueA (L3.IntV i1), L3.ValueA (L3.IntV i2)) -> L3.ValueA (L3.IntV (i1 * i2))
    _ -> L3.BadA
  L3.BoolE b -> L3.ValueA (L3.BoolV b)
  L3.IfE e1 e2 e3 -> case interpEnv env e1 of
    L3.ValueA (L3.BoolV b) ->
      if b 
      then interpEnv env e2 
      else interpEnv env e3
    _ -> L3.BadA
  L3.VarE x -> case Map.lookup x env of
    Just v -> L3.ValueA v
    _ -> L3.BadA
  L3.LetE x e1 e2 -> case interpEnv env e1 of
    L3.ValueA v -> interpEnv (Map.insert x v env) e2
    _ -> L3.BadA

testX2 :: Test
testX2 = TestDir
  ( "X2"
  , "interpEnv"
  , uncurry interpEnv
  , "tests/hw04/x2"
  , parseTest (pPair L3.pEnv L3.pExpr) L3.pAnswer
  )

----------
-- [X3] --
----------

-- Convert a value to an expression.

exprFromValue :: L3.Value -> L3.Expr
exprFromValue e = case e of
  L3.IntV i -> L3.IntE i
  L3.BoolV b -> L3.BoolE b

testX3 :: Test
testX3 = TestDir
  ( "X3"
  , "exprFromValue"
  , exprFromValue
  , "tests/hw04/x3"
  , parseTest L3.pValue L3.pExpr
  )

-- ========== --
-- ASSIGNMENT --
-- ========== --

---------------
-- [E1]: ★☆☆ --
---------------

-- Write a free variable analysis for L3.

fv :: L3.Expr -> Set String
fv e = error "TODO"

testE1 :: Test
testE1 = TestDir
  ( "E1"
  , "fv"
  , fv
  , "tests/hw04/e1"
  , parseTest L3.pExpr (pSet pVar)
  )

---------------
-- [E2]: ★★☆ --
---------------

-- Write a substitution function for L3.
--
-- `subst x e1 e2` should return an expression just like e2, but with all
-- occurrences of x replaced with the term e1.
--
-- Assume that the expression being substituted is closed.

subst :: String -> L3.Expr -> L3.Expr -> L3.Expr
subst x e1 e2 = error "TODO"

testE2 :: Test
testE2 = TestDir
  ( "E2"
  , "subst"
  , uncurry $ uncurry subst
  , "tests/hw04/e2"
  , parseTest (pPair (pPair pVar L3.pExpr) L3.pExpr) L3.pExpr
  )

---------------
-- [E3]: ★★★ --
---------------

-- Write a call-by-value interpreter for L3 that uses `subst` (your solution to
-- E2). You should also use `exprFromValue` (the example X3).

interpSubst :: L3.Expr -> L3.Answer
interpSubst e = error "TODO"
    
testE3 :: Test
testE3 = TestDir
  ( "E3"
  , "interpSubst"
  , interpSubst
  , "tests/hw04/e3"
  , parseTest L3.pExpr L3.pAnswer
  )

main :: IO ()
main = runTests 
  -- examples
  [ testX1
  , testX2
  , testX3
  -- assignment
  , testE1
  , testE2
  , testE3
  ]
