module HW07 where

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

import Data.Map (Map)
import qualified Data.Map as Map

import Util.Testing

import Util.Lex

import qualified Lang.L6 as L6

-- ========== --
-- SUBMITTING --
-- ========== --

-- How to submit:
--
-- Submit a copy of this file only “HWXX.hs” (for XX = 01, 02, etc.) to
-- gradescope.

-- ======== --
-- REFERECE --
-- ======== --

-- Here is a copy of of Lang.L6.Data for your reference:

{-
-----------------
-- EXPRESSIONS --
-----------------

data Expr = 
    IntE Integer
  | PlusE Expr Expr 
  | TimesE Expr Expr
  | BoolE Bool
  | IfE Expr Expr Expr
  | VarE String
  | LetE String Expr Expr
  | FunE String Expr
  | AppE Expr Expr
  -- NEW
  | BoxE Expr
  | UnboxE Expr
  | AssignE Expr Expr
  deriving (Eq,Ord,Show)

---------------
-- SEMANTICS --
---------------

data Value = 
    IntV Integer
  | BoolV Bool
  | CloV String Expr Env
  -- NEW
  | LocV Integer
  deriving (Eq,Ord,Show)

-- NEW
type Answer = Maybe (Store, Value)

type Env = Map String Value

-- NEW
type Store = Map Integer Value
-}

-- ======== --
-- EXAMPLES --
-- ======== --

----------
-- [X1] --
----------

-- Define a function that counts up from [1..n] for some n

countDown :: Integer -> [Integer]
countDown i =
  if i <= 0 
     then []
     else i : countDown (i - 1)

-- Define a function that counts down from [n..1] for some n

countUp :: Integer -> [Integer] -> [Integer]
countUp i is =
  if i <= 0
     then is
     else countUp (i - 1) (i : is)

testX1A :: Test
testX1A = Test1
  ( "X1A"
  , "countDown"
  , countDown
  , [ (0,  [])
    , (1,  [1])
    , (2,  [2,1])
    , (5,  [5,4,3,2,1])
    ]
  )

testX1B :: Test
testX1B = Test1
  ( "X1B"
  , "countUp"
  , (\ i -> countUp i [])
  , [ (0,  [])
    , (1,  [1])
    , (2,  [1,2])
    , (5,  [1,2,3,4,5])
    ]
  )


----------
-- [X2] -- 
----------

-- Define a function that deletes the three smallest keys from a map, and
-- returns the sum of values mapped to those keys, along with the modified map.

stateManipulation :: Map String Integer -> Maybe (Integer, Map String Integer)
stateManipulation sto =
  case Map.minView sto of
    Just (x, sto') -> case Map.minView sto' of
      Just (y, sto'') -> case Map.minView sto'' of
        Just (z, sto''') -> Just (x + y + z, sto''')
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing

testX2 :: Test
testX2 = Test1
  ( "X2"
  , "stateManipulation"
  , stateManipulation
  , [ ( Map.fromList [("W", 1), ("A", 2), ("L", 3), ("T", 4), ("E", 5), ("R", 6)]
      , Just (10, Map.fromList [("W", 1), ("T", 4), ("R", 6)]))
    , ( Map.fromList [("Walter", 1), ("Camilla", 2), ("David", 3), ("Willow", 4)]
      , Just (6, Map.fromList [("Willow", 4)]))
    ]
  )

----------
-- [X3] --
----------

-- Define a function which computes the free variables of an expression in our
-- new language which contains features for mutable state and first class
-- references.

freeVars :: L6.Expr -> Set String
freeVars e = case e of
  L6.IntE _i -> Set.empty
  L6.PlusE e1 e2 -> Set.union (freeVars e1) (freeVars e2)
  L6.TimesE e1 e2 -> Set.union (freeVars e1) (freeVars e2)
  L6.BoolE _b -> Set.empty
  L6.IfE e1 e2 e3 -> Set.union (freeVars e1) (Set.union (freeVars e2) (freeVars e3))
  L6.VarE x -> Set.singleton x
  L6.LetE x e1 e2 -> Set.union (freeVars e1) (Set.delete x (freeVars e2))
  L6.FunE x e' -> Set.delete x (freeVars e')
  L6.AppE e1 e2 -> Set.union (freeVars e1) (freeVars e2)
  L6.BoxE e' -> freeVars e'
  L6.UnboxE e' -> freeVars e'
  L6.AssignE e1 e2 -> Set.union (freeVars e1) (freeVars e2)

testX3 :: Test
testX3 = TestDir
  ( "X3"
  , "freeVars"
  , freeVars
  , "tests/hw07/x3"
  , parseTest L6.pExpr (pSet pVar)
  )

-- ========== --
-- ASSIGNMENT --
-- ========== --

---------------
-- [E1]: ★☆☆ --
---------------

-- Implement a function which takes a list of integers as input and returns the
-- smallest nonnegative integer which is larger than every integer in the list,
-- or 0 if there are no elements in the list. 
--
-- A spec for the function is as follows:
--
-- fresh-integer ∈ list(ℤ) → ℤ
-- fresh-integer [i₁,…,iₙ] ≜ i
--   where
--     i = MAX [0,n₁+1,…,nₙ+1]
--
-- HINT: implement this function by recursion on the list of integers

freshInteger :: [Integer] -> Integer
freshInteger is = error "TODO"

testE1 :: Test
testE1 = Test1
  ( "E1"
  , "freshInteger"
  , freshInteger
  , [ ([], 0)
    , ([1], 2)
    , ([1,2,3], 4)
    , ([-12], 0)
    ]
  )

---------------
-- [E2]: ★☆☆ --
---------------

-- Implement a function which takes a store and returns a fresh location which
-- is not currently used in the domain of the store.
--
-- A spec for the function is as follows:
--
-- fresh-location ∈ store → loc
-- fresh-location(σ) ≜ ℓ
--   where
--     ℓ ∉ dom(σ)
--
-- HINTS: 
-- - use `Map.keys :: Map k v -> [k]`
-- - use `freshInteger` (E1)

freshLocation :: L6.Store -> Integer
freshLocation sto = error "TODO"

testE2 :: Test
testE2 = Test1
  ( "E2"
  , "freshLocation"
  , freshLocation
  , [ (Map.fromList [], 0)
    , (Map.fromList [(1, L6.IntV 0), (2, L6.IntV 0)], 3)
    , (Map.fromList [(100, L6.IntV 0), (200, L6.IntV 0), (300, L6.IntV 0)], 301)
   ]
  )

---------------
-- [E3]: ★★★ --
---------------

-- Implement an interpreter for L6, the language with state mutation and first
-- class references.
--
-- Where the following spec is not defined, your implementation should return
-- `Nothing`.
--
-- A spec for the function is as follows:
--
-- ⟦_⟧ ∈ expr → (env × store ⇀ value × store)
-- ⟦i⟧(γ,σ) ≜ (i,σ)
-- ⟦e₁ + e₂⟧(γ,σ) ≜ (i₁ + i₂,σ″)
--   where (i₁,σ′) = ⟦e₁⟧(γ,σ)
--         (i₂,σ″) = ⟦e₁⟧(γ,σ′)
-- ⟦e₁ * e₂⟧(γ,σ) ≜ (i₁ * i₂,σ″)
--   where (i₁,σ′) = ⟦e₁⟧(γ,σ)
--         (i₂,σ″) = ⟦e₁⟧(γ,σ′)
-- ⟦b⟧(γ,σ) ≜ (b,σ)
-- ⟦if e₁ then e₂ else e₃⟧(γ,σ) ≜ ⟦e₂⟧(γ,σ′)
--   when (true,σ′) = ⟦e₁⟧(γ,σ)
-- ⟦if e₁ then e₂ else e₃⟧(γ,σ) ≜ ⟦e₃⟧(γ,σ′)
--   when (false,σ′) = ⟦e₁⟧(γ,σ)
-- ⟦x⟧(γ,σ) ≜ (v,σ)
--   where v = γ(x)
-- ⟦let x = e₁ in e₂⟧(γ,σ) ≜ ⟦e₂⟧(γ′,σ′)
--   where (v,σ′) = ⟦e₁⟧(γ,σ)
--         γ′     = γ[x↦v]
-- ⟦fun x ⇒ e⟧(γ,σ) ≜ (⟨fun x ⇒ e , γ ⟩, σ)
-- ⟦e₁ e₂⟧(γ,σ) ≜ ⟦e⟧(γ″,σ″)
--   where (⟨ fun x ⇒ e , γ′ ⟩,σ′) = ⟦e₁⟧(γ,σ)
--         (v,σ″)                  = ⟦e₂⟧(γ,σ′)
--         γ″                      = γ′[x↦v]
-- ⟦box e⟧(γ,σ) ≜ (ℓ, σ″)
--   where (v,σ′) = ⟦e⟧(γ,σ)
--         σ″     = σ′[ℓ↦v]
-- ⟦!e⟧(γ,σ) ≜ (v,σ′)
--   where (ℓ,σ′) = ⟦e⟧(γ,σ)
--         v      = σ′(ℓ)
-- ⟦e₁ ← e₂⟧(γ,σ) ≜ (v,σ‴)
--   where (ℓ,σ′) = ⟦e₁⟧(γ,σ)
--         (v,σ″) = ⟦e₂⟧(γ,σ′)
--         σ‴     = σ″[ℓ↦v]

interp :: L6.Expr -> L6.Env -> L6.Store -> Maybe (L6.Value, L6.Store)
interp e env sto = case e of
  L6.IntE i -> Just (L6.IntV i, sto)
  L6.PlusE e1 e2 -> case interp e1 env sto of
    Just (L6.IntV i1, sto') -> case interp e2 env sto' of
      Just (L6.IntV i2, sto'') -> Just (L6.IntV (i1 + i2), sto'')
      _ -> Nothing
    _ -> Nothing
  L6.TimesE e1 e2 -> case interp e1 env sto of
    Just (L6.IntV i1, sto') -> case interp e2 env sto' of
      Just (L6.IntV i2, sto'') -> Just (L6.IntV (i1 * i2), sto'')
      _ -> Nothing
    _ -> Nothing
  L6.BoolE b -> Just (L6.BoolV b, sto)
  L6.IfE e1 e2 e3 -> case interp e1 env sto of
    Just (L6.BoolV b, sto') ->
      if b 
         then interp e2 env sto'
         else interp e3 env sto'
    _ -> Nothing
  L6.VarE x -> case Map.lookup x env of
    Just v -> Just (v, sto)
    _ -> Nothing
  L6.LetE x e1 e2 -> case interp e1 env sto of
    Just (v, sto') -> interp e2 (Map.insert x v env) sto'
    _ -> Nothing
  L6.FunE x e' -> error "TODO"
  L6.AppE e1 e2 -> error "TODO"
  L6.BoxE e' -> error "TODO"
  L6.UnboxE e' -> error "TODO"
  L6.AssignE e1 e2 -> error "TODO"

testE3 :: Test
testE3 = TestDir
  ( "E3"
  , "interp"
  , (\ e -> interp e Map.empty Map.empty)
  , "tests/hw07/e3"
  , parseTest L6.pExpr (pMaybe (pPair L6.pValue L6.pStore))
  )

main :: IO ()
main = runTests 
  -- examples
  [ testX1A
  , testX1B
  , testX2
  , testX3
  -- assignment
  , testE1
 , testE2
 , testE3
  ]
