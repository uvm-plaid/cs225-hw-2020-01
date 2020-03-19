module HW06 where

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

import qualified Lang.L5 as L5

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

-- Here is a copy of the contents of Lang.L5.Data for your reference:

-- -----------------
-- -- EXPRESSIONS --
-- -----------------
--
-- data Expr = 
--     IntE Integer
--   | PlusE Expr Expr 
--   | TimesE Expr Expr
--   | BoolE Bool
--   | IfE Expr Expr Expr
--   | VarE String
--   | LetE String Expr Expr
--   -- NEW
--   | FunE String Expr
--   | AppE Expr Expr
--   deriving (Eq,Ord,Show)
-- 
-- ---------------------------
-- -- ENVIRONMENT SEMANTICS --
-- ---------------------------
-- 
-- data ValueE = 
--     IntEV Integer
--   | BoolEV Bool
--   -- NEW
--   | CloEV String Expr EnvE
--   deriving (Eq,Ord,Show)
-- 
-- data AnswerE = 
--     ValueEA ValueE
--   | BadEA
--   deriving (Eq,Ord,Show)
-- 
-- type EnvE = Map String ValueE
-- 
-- ----------------------------
-- -- SUBSTITUTION SEMANTICS --
-- ----------------------------
-- 
-- data ValueS =
--     IntSV Integer
--   | BoolSV Bool
--   | FunSV String Expr
--   deriving (Eq,Ord,Show)
-- 
-- data AnswerS =
--     ValueSA ValueS
--   | BadSA
--   deriving (Eq,Ord,Show)
-- 
-- type EnvS = Map String ValueS

-- ======== --
-- EXAMPLES --
-- ======== --

----------
-- [X1] --
----------

-- Convert a (substitution-based) value into an expression.

exprFromValue :: L5.ValueS -> L5.Expr
exprFromValue v = case v of
  L5.IntSV i -> L5.IntE i
  L5.BoolSV b -> L5.BoolE b
  L5.FunSV x e -> L5.FunE x e


testX1 :: Test
testX1 = TestDir
  ( "X1"
  , "exprFromValue"
  , exprFromValue
  , "tests/hw06/x1"
  , parseTest L5.pValueS L5.pExpr
  )

----------
-- [X2] -- 
----------

-- Compute the free variables of an expression.

freeVarsExpr :: L5.Expr -> Set String
freeVarsExpr e = case e of 
  L5.IntE _i -> Set.empty
  L5.PlusE e1 e2 -> Set.union (freeVarsExpr e1) (freeVarsExpr e2)
  L5.TimesE e1 e2 -> Set.union (freeVarsExpr e1) (freeVarsExpr e2)
  L5.BoolE _b -> Set.empty
  L5.IfE e1 e2 e3 -> Set.union (freeVarsExpr e1) (Set.union (freeVarsExpr e2) (freeVarsExpr e3))
  L5.VarE x -> Set.singleton x
  L5.LetE x e1 e2 -> Set.union (freeVarsExpr e1) (Set.delete x (freeVarsExpr e2))
  L5.FunE x e' -> Set.delete x (freeVarsExpr e')
  L5.AppE e1 e2 -> Set.union (freeVarsExpr e1) (freeVarsExpr e2)


testX2 :: Test
testX2 = TestDir
  ( "X2"
  , "freeVarsExpr"
  , freeVarsExpr
  , "tests/hw06/x2"
  , parseTest L5.pExpr (pSet pVar)
  )

----------
-- [X3] --
----------

freeVarsVal :: L5.ValueS -> Set String
freeVarsVal v = case v of
  L5.IntSV _i -> Set.empty
  L5.BoolSV _b -> Set.empty
  L5.FunSV x e -> Set.delete x (freeVarsExpr e)

testX3 :: Test
testX3 = TestDir
  ( "X3"
  , "freeVarsVal"
  , freeVarsVal
  , "tests/hw06/x3"
  , parseTest L5.pValueS (pSet pVar)
  )

-- ========== --
-- ASSIGNMENT --
-- ========== --

---------------
-- [E1]: ★★☆ --
---------------

-- Implement substitution of values inside of expressions in L5, the language
-- with first class functions.
--
-- `substValue x v e` should replace all free occurrences of the variable `x`
-- with `v` inside the expression `e`. This operation is notated `[v/x]e` in
-- math.
--
-- A full specification for this function is in the course notes, and
-- reproduced here in unicode:
--
-- [_/_]_ ∈ value × var × expr → expr
-- [v/x]i ≜ i
-- [v/x](e₁ + e₂) ≜ [v/x]e₁ + [v/x]e₂
-- [v/x](e₁ * e₂) ≜ [v/x]e₁ * [v/x]e₂
-- [v/x]b ≜ b
-- [v/x](if e₁ then e₂ else e₃) ≜ if [v/x]e₁ then [v/x]e₂ else [v/x]e₃
-- [v/x]y ≜ v    when x = y
-- [v/x]y ≜ y    when x ≠ y
-- [v/x](let y = e₁ in e₂) ≜ let y = [v/x]e₁ in e₂         when x = y
-- [v/x](let y = e₁ in e₂) ≜ let y = [v/x]e₁ in [v/x]e₂    when x ≠ y
-- [v/x](fun y ⇒ e) ≜ fun y ⇒ e         when x = y
-- [v/x](fun y ⇒ e) ≜ fun y ⇒ [v/x]e    when x ≠ y
-- [v/x](e₁ e₂) ≜ [v/x]e₁ [v/x]e₂
--
-- HINTS:
-- - Use `exprFromValue` (X1) to convert from a value to an expression in the
--   `VarE` case
-- - Don't forget to cover all of the cases! If the above definition is
--   undefined anywhere, your Haskell function should return `bad`.

substValue :: String -> L5.ValueS -> L5.Expr -> L5.Expr
substValue x v e = case e of
  L5.IntE i -> L5.IntE i
  L5.PlusE e1 e2 -> L5.PlusE (substValue x v e1) (substValue x v e2)
  L5.TimesE e1 e2 -> L5.TimesE (substValue x v e1) (substValue x v e2)
  L5.BoolE b -> L5.BoolE b
  L5.IfE e1 e2 e3 -> L5.IfE (substValue x v e1) (substValue x v e2) (substValue x v e3)
  L5.VarE y ->
    if x == y
       then exprFromValue v
       else L5.VarE y
  L5.LetE y e1 e2 -> error "TODO"
  L5.FunE y e' -> error "TODO"
  L5.AppE e1 e2 -> error "TODO"

testE1 :: Test
testE1 = TestDir
  ( "E1"
  , "substValue"
  , uncurry (uncurry substValue)
  , "tests/hw06/e1"
  , parseTest (pPair (pPair pVar L5.pValueS) L5.pExpr) L5.pExpr
  )

---------------
-- [E2]: ★★☆ --
---------------

-- Implement a substitution-based interpreter for L5, the language with first
-- class functions.
--
-- `interpWithSubst e` should evaluate to the result of interpreting the
-- expression `e`. This operation is notated `⟦e⟧` in math.
--
-- A full specification for this function is in the course notes, and
-- reproduced here in unicode:
--
-- ⟦_⟧ ∈ expr → answer
-- ⟦i⟧ ≜ i
-- ⟦e₁ + e₂⟧ ≜ i₁ + i₂
--   where i₁ = ⟦e₁⟧
--         i₂ = ⟦e₂⟧
-- ⟦e₁ * e₂⟧ ≜ i₁ * i₂
--   where i₁ = ⟦e₁⟧
--         i₂ = ⟦e₂⟧
-- ⟦b⟧ ≜ b
-- ⟦if e₁ then e₂ else e₃⟧ ≜ ⟦e₂⟧    if ⟦e₁⟧ = true
-- ⟦if e₁ then e₂ else e₃⟧ ≜ ⟦e₃⟧    if ⟦e₁⟧ = false
-- ⟦let x = e₁ in e₂⟧ ≜ ⟦[v/x]e₂⟧
--   where v = ⟦e₁⟧
-- ⟦fun y ⇒ e⟧ ≜ fun y ⇒ e    if FV(e) ⊆ {y}
-- ⟦e₁ e₂⟧ ≜ ⟦[v/x]e′⟧
--   where fun x ⇒ e′ = ⟦e₁⟧
--         v          = ⟦e₂⟧
--
-- HINTS:
-- - Note that the usual constructors `ValueA`, `IntV`, etc. are renamed to
--   `ValueSA` and `IntSV`, where the mnemonic is “Value Substitution-based
--   Answer” and “Int Substitution-based Value”, and they are so named to
--   distinguish them from the environment-based answers and values.
-- - Use `freeVarsExpr` (X2) to compute the free varaibles check in the `fun y ⇒ e`
--   case.
-- - Use `Set.isSubsetOf :: Set a -> Set a -> Bool` to check if one set is a
--   subset (⊆) of another.
-- - Don't forget to cover all of the cases! If the above definition is
--   undefined anywhere, your Haskell function should return `bad`.

interpWithSubst :: L5.Expr -> L5.AnswerS
interpWithSubst e = case e of
  L5.IntE i -> L5.ValueSA (L5.IntSV i)
  L5.PlusE e1 e2 -> case (interpWithSubst e1, interpWithSubst e2) of
    (L5.ValueSA (L5.IntSV i1), L5.ValueSA (L5.IntSV i2)) -> L5.ValueSA (L5.IntSV (i1 + i2))
    _ -> L5.BadSA
  L5.TimesE e1 e2 -> case (interpWithSubst e1, interpWithSubst e2) of
    (L5.ValueSA (L5.IntSV i1), L5.ValueSA (L5.IntSV i2)) -> L5.ValueSA (L5.IntSV (i1 * i2))
    _ -> L5.BadSA
  L5.BoolE b -> L5.ValueSA (L5.BoolSV b)
  L5.IfE e1 e2 e3 -> case interpWithSubst e1 of
    L5.ValueSA (L5.BoolSV b) ->
      if b 
         then interpWithSubst e2
         else interpWithSubst e3
    _ -> L5.BadSA
  L5.VarE _x -> L5.BadSA
  L5.LetE x e1 e2 -> error "TODO"
  L5.FunE x e -> error "TODO"
  L5.AppE e1 e2 -> error "TODO"

testE2 :: Test
testE2 = TestDir
  ( "E2"
  , "interpWithSubst"
  , interpWithSubst
  , "tests/hw06/e2"
  , parseTest L5.pExpr L5.pAnswerS
  )

---------------
-- [E3]: ★★☆ --
---------------

-- Implement an environment-based interpreter for L5, the language with first
-- class functions.
--
-- `interpWithEnv env e` should evaluate the expression `e` inside of the
-- (delayed substitution) environment `env`. This operation is written `⟦e⟧(γ)`
-- in math where `γ` is the environment `env`.
--
-- A full specification for this function is in the course notes, and
-- reproduced here in unicode:
--
-- ⟦_⟧ ∈ expr → env → answer
-- ⟦i⟧(γ) ≜ i
-- ⟦e₁ + e₂⟧(γ) ≜ i₁ + i₂
--   where i₁ = ⟦e₁⟧(γ)
--         i₂ = ⟦e₂⟧(γ)
-- ⟦e₁ * e₂⟧(γ) ≜ i₁ * i₂
--   where i₁ = ⟦e₁⟧(γ)
--         i₂ = ⟦e₂⟧(γ)
-- ⟦b⟧(γ) ≜ b
-- ⟦if e₁ then e₂ else e₃⟧(γ) ≜ ⟦e₂⟧(γ)    if ⟦e₁⟧(γ) = true
-- ⟦if e₁ then e₂ else e₃⟧(γ) ≜ ⟦e₃⟧(γ)    if ⟦e₁⟧(γ) = false
-- ⟦let x = e₁ in e₂⟧(γ) ≜ ⟦e₂⟧(γ[x↦v])
--   where v = ⟦e₁⟧(γ)
-- ⟦fun y ⇒ e⟧ ≜ (fun y ⇒ e, γ)
-- ⟦e₁ e₂⟧ ≜ ⟦e′⟧(γ′[x↦v])
--   where (fun x ⇒ e′, γ′) = ⟦e₁⟧(γ)
--         v                = ⟦e₂⟧(γ)
--
-- HINTS:
-- - Don't forget to cover all of the cases! If the above definition is
--   undefined anywhere, your Haskell function should return `bad`.

interpWithEnv :: L5.EnvE -> L5.Expr -> L5.AnswerE
interpWithEnv env e = case e of
  L5.IntE i -> L5.ValueEA (L5.IntEV i)
  L5.PlusE e1 e2 -> case (interpWithEnv env e1, interpWithEnv env e2) of
    (L5.ValueEA (L5.IntEV i1), L5.ValueEA (L5.IntEV i2)) -> L5.ValueEA (L5.IntEV (i1 + i2))
    _ -> L5.BadEA
  L5.TimesE e1 e2 -> case (interpWithEnv env e1, interpWithEnv env e2) of
    (L5.ValueEA (L5.IntEV i1), L5.ValueEA (L5.IntEV i2)) -> L5.ValueEA (L5.IntEV (i1 * i2))
    _ -> L5.BadEA
  L5.BoolE b -> L5.ValueEA (L5.BoolEV b)
  L5.IfE e1 e2 e3 -> case interpWithEnv env e1 of
    L5.ValueEA (L5.BoolEV b) ->
      if b 
         then interpWithEnv env e2
         else interpWithEnv env e3
    _ -> L5.BadEA
  L5.VarE x -> case Map.lookup x env of
    Just v -> L5.ValueEA v
    Nothing -> L5.BadEA
  L5.LetE x e1 e2 -> error "TODO"
  L5.FunE x e -> error "TODO"
  L5.AppE e1 e2 -> error "TODO"

testE3 :: Test
testE3 = TestDir
  ( "E3"
  , "interpWithEnv"
  , interpWithEnv Map.empty
  , "tests/hw06/e3"
  , parseTest L5.pExpr L5.pAnswerE
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
