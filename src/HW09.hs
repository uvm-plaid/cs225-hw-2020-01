module HW09 where

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

import qualified Lang.L8 as L8

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

-- Recall the mathematical definitions of abstract integers, booleans, values
-- and answers.
--
--     lb ∈ LB ⩴ -∞ | i
--     ub ∈ UB ⩴ i | +∞
--     î ∈ ℤ̂ ⩴ ⊥ | [lb,ub]  where  lb ≤ ub
--     b̂ ∈ 𝔹̂ ≜ ℘(𝔹)
--     v̂ ∈ v̂alue ⩴ ⟨î,b̂⟩
--     â ∈ ânswer ⩴ ⟨b,v̂⟩
--                   ↑
--                   failure bit

--------------------------
-- [E1]: ★★☆ (integers) --
--------------------------

-- Implement `joinInteger`, the join operation on abstract integers,
-- `plusInteger`, the plus operation on abstract integers, and `plusHatAnswer`,
-- the plus operation on abstract answers. Note that many definitions here are
-- defined for you; you should familiarize yourself with them.

-- The bottom element for abstract integers.
--
--     ⊥ ∈ ℤ̂
--     ⊥ ≜ ⊥
--
botInteger :: L8.IntegerHat
botInteger = L8.BotIH

-- (E1.A)
-- The join operation for abstract integers.
--
--     _⊔_ ∈ ℤ̂ × ℤ̂ → ℤ̂
--     ⊥         ⊔ î₂        ≜ î₂
--     î₁        ⊔ ⊥         ≜ î₁
--     [lb₁,ub₁] ⊔ [lb₂,ub₂] ≜ [min(lb₁,lb₂),max(ub₁,ub₂)]
--
-- HINTS:
-- - Do pattern matching on both integer arguments to the function; you should
--   have 3 cases, just like the math above.
-- - You can use Haskell functions for min and max:
--       
--       min :: Integer -> Integer -> Integer
--       max :: Integer -> Integer -> Integer
--
-- - Note that the math syntax `[lb,ub]` is a range (a pair of two numbers---a
--   kind of abstract integer), and not a list of two elements.
joinInteger :: L8.IntegerHat -> L8.IntegerHat -> L8.IntegerHat
joinInteger i1 i2 = error "TODO"

testE1A :: Test
testE1A = TestDir
  ( "E1.A"
  , "joinInteger"
  , uncurry joinInteger
  , "tests/hw09/e1a"
  , parseTest (pPair L8.pIntHat L8.pIntHat) L8.pIntHat
  )

-- The injection operation for abstract integers.
--
--     ⌊_⌋ ∈ ℤ → ℤ̂
--     ⌊i⌋ ≜ [i,i]
--
injectInteger :: Integer -> L8.IntegerHat
injectInteger i = L8.RangeIH i i

-- (E1.B)
-- The plus operation for abstract integers.
--
--     _+̂_ ∈ ℤ̂ × ℤ̂ → ℤ̂
--     î₁        +̂ ⊥         ≜ ⊥
--     ⊥         +̂ î₂        ≜ ⊥
--     [lb₁,ub₁] +̂ [lb₂,ub₂] ≜ [lb₁+lb₂,ub₁+ub₂]
--
-- HINTS:
-- - do pattern matching on both integer arguments to the function; you should
--   have 3 cases, just like the math above.
plusHat :: L8.IntegerHat -> L8.IntegerHat -> L8.IntegerHat
plusHat i1 i2 = error "TODO"

testE1B :: Test
testE1B = TestDir
  ( "E1.B"
  , "plusHat"
  , uncurry plusHat
  , "tests/hw09/e1b"
  , parseTest (pPair L8.pIntHat L8.pIntHat) L8.pIntHat
  )

-- (E1.C)
-- The plus operation for abstract answers.
--
--     _+̂_ ∈ ânswer × ânswer → ânswer
--     ⟨b₁,î₁,b̂₁⟩ +̂ ⟨b₂,î₂,b̂₂⟩ ≜ ⟨b′,î′,b̂′⟩
--       where b′ = b₁ ∨ b₂ ∨ (b̂₁ ≠ ∅) ∨ (b̂₂ ≠ ∅)
--             î′ = î₁ +̂ î₂
--             b̂′ = ∅
--
-- HINTS:
-- - Note the Haskell syntax for multiple let-bindings:
--   
--       let x = stuff
--           y = more stuff
--           z = even more stuff
--       in
--       final value
-- - You can use the Haskell operator for logical disjunction:
--
--       (||) :: Bool -> Bool -> Bool
--
plusHatAnswer :: L8.AnswerHat -> L8.AnswerHat -> L8.AnswerHat
plusHatAnswer (L8.AnswerHat d1 (L8.ValueHat i1 b1)) (L8.AnswerHat d2 (L8.ValueHat i2 b2)) = error "TODO"

testE1C :: Test
testE1C = TestDir
  ( "E1.C"
  , "plusHatAnswer"
  , uncurry plusHatAnswer
  , "tests/hw09/e1c"
  , parseTest (pPair L8.pAnswerHat L8.pAnswerHat) L8.pAnswerHat
  )

--------------------------
-- [E2]: ★★☆ (booleans) --
--------------------------

-- Implement `condHatAnswer`, the conditional operation on abstract answers.

-- The bottomm element for abstract booleans.
--
--     ⊥ ∈ 𝔹̂
--     ⊥ ≜ ∅
--
botBool :: L8.BoolHat
botBool = Set.empty

-- The join operation for abstract booleans.
--
--     _⊔_ ∈ 𝔹̂ × 𝔹̂ → 𝔹̂
--     b̂₁ ⊔ b̂₂ ≜ b̂₁ ∪ b̂₂
--
joinBool :: L8.BoolHat -> L8.BoolHat -> L8.BoolHat
joinBool b1 b2 = Set.union b1 b2

-- The injection operation for abstract booleans.
--
--     ⌊_⌋ ∈ 𝔹 → 𝔹̂
--     ⌊b⌋ ≜ {b}
--
injectBool :: Bool -> L8.BoolHat
injectBool b = Set.singleton b

-- (E2)
-- The conditional operation for abstract answers.
--
--
--     ĉond ∈ ânswer × ânswer × ânswer → ânswer
--     ĉond(⟨b₁,î₁,b̂₁⟩,â₂,â₃) ≜ â₁′ ⊔ â₂′ ⊔ â₃′
--       where â₁′ = ⟨ b₁ ∨ (î₁ ≠ ⊥) , ⊥ ⟩
--             â₂′ = â₂  if true ∈ b̂₁
--             â₂′ = ⊥   if true ∉ b̂₁
--             â₃′ = â₃  if false ∈ b̂₁
--             â₃′ = ⊥   if false ∈ b̂₁
--
-- HINTS:
-- - You may want to use `joinAnswers`, which takes a list of answers, and
--   joins them all together using `joinAnswer`. E.g., you can call joinAnswers
--   on a Haskell list of three elements `[a, b, c]` to join three abstract
--   answers together.
-- - You can use the Haskell operator `||` for logical disjunction, and the
--   function `not` for negation:
--
--       (||) :: Bool -> Bool -> Bool
--       not :: Bool -> Bool
--
-- - You can use `Set.member` to test if an element is in a set:
--
--       Set.member :: a -> Set a -> Bool
--
condHatAnswer :: L8.AnswerHat -> L8.AnswerHat -> L8.AnswerHat -> L8.AnswerHat
condHatAnswer (L8.AnswerHat d1 (L8.ValueHat i1 b1)) a2 a3 = error "TODO"

testE2 :: Test
testE2 = TestDir
  ( "E2"
  , "condHatAnswer"
  , uncurry (uncurry condHatAnswer)
  , "tests/hw09/e2"
  , parseTest (pPair (pPair L8.pAnswerHat L8.pAnswerHat) L8.pAnswerHat) L8.pAnswerHat
  )

------------
-- VALUES --
------------

-- The bottom element for abstract values.
--
--     ⊥ ∈ v̂alue
--     ⊥ ≜ ⟨⊥,⊥⟩
--
botValue :: L8.ValueHat
botValue = L8.ValueHat botInteger botBool

-- The join operation for abstract values.
--
--     _⊔_ ∈ v̂alue × v̂alue → v̂alue
--     ⟨î₁,b̂₁⟩ ⊔ ⟨î₂,b̂₂⟩ ≜ ⟨î₁⊔î₂,b̂₁⊔b̂₂⟩
--
joinValue :: L8.ValueHat -> L8.ValueHat -> L8.ValueHat
joinValue (L8.ValueHat i1 b1) (L8.ValueHat i2 b2) = L8.ValueHat (joinInteger i1 i2) (joinBool b1 b2)

-- The injection operation for abstract values
--
--     ⌊_⌋ ∈ value → v̂alue
--     ⌊i⌋ ≜ ⟨⌊i⌋,⊥⟩
--     ⌊b⌋ ≜ ⟨⊥,⌊b⌋⟩
--
injectValue :: L8.Value -> L8.ValueHat
injectValue v = case v of
  L8.IntV i -> L8.ValueHat (injectInteger i) botBool
  L8.BoolV b -> L8.ValueHat botInteger (injectBool b)

-------------
-- ANSWERS --
-------------

-- The bottom element for abstract answers.
--
--     ⊥ ∈ ânswer
--     ⊥ ≜ ⟨false,⊥⟩
--
botAnswer :: L8.AnswerHat
botAnswer = L8.AnswerHat False botValue

-- The join operation for abstract answers.
--
--     _⊔_ ∈ ânswer × ânswer → ânswer
--     ⟨b₁,v̂₁⟩ ⊔ ⟨b₂,v̂₂⟩ = ⟨b₁∨b₂,v̂₁⊔v̂₂⟩
--
joinAnswer :: L8.AnswerHat -> L8.AnswerHat -> L8.AnswerHat
joinAnswer (L8.AnswerHat d1 v1) (L8.AnswerHat d2 v2) = L8.AnswerHat (d1 || d2) (joinValue v1 v2)

-- Combine a list of answers using join, with the neutral element as bottom.
--
--     joins ∈ list(ânswer) → ânswer
--     joins(â₁,…,âₙ) ≜ â₁ ⊔ ⋯ ⊔ âₙ
joinAnswers :: [L8.AnswerHat] -> L8.AnswerHat
joinAnswers = foldl joinAnswer botAnswer

-- Lift an abstract value to an abstract answer.
--
--     lift ∈ v̂alue → ânswer
--     lift(v̂) ≜ ⟨false,v̂⟩
--
liftValueAnswer :: L8.ValueHat -> L8.AnswerHat
liftValueAnswer v = L8.AnswerHat False v

-- The injection operation for abstract answers.
--
--     ⌊_⌋ ∈ answer → ânswer
--     ⌊v⌋ ≜ lift(⌊v⌋)
--     ⌊bad⌋ ≜ lift-fail(true)
--
injectAnswer :: L8.Answer -> L8.AnswerHat
injectAnswer a = case a of
  Just v -> liftValueAnswer (injectValue v)
  Nothing -> L8.AnswerHat True botValue

-----------------
-- INTERPRETER --
-----------------

---------------
-- [E3]: ★★☆ --
---------------

-- Implement the abstract interpreter, `interpHat`, using functions defined
-- above.
--
--     ⟦_⟧(_) ∈ expr → ênv → ânswer
--     ⟦i⟧(γ̂) ≜ ⌊i⌋
--     ⟦e₁+e₂⟧(γ̂) ≜ ⟦e₁⟧(γ̂) +̂ ⟦e₂⟧(γ̂)
--     ⟦b⟧(γ̂) ≜ ⌊b⌋
--     ⟦if e₁ then e₂ else e₃⟧(γ̂) ≜ ĉond(⟦e₁⟧(γ̂),⟦e₂⟧(γ̂),⟦e₃⟧(γ̂))
--     ⟦x⟧(γ̂) ≜ lift(γ̂(x))     if x ∈ γ
--     ⟦x⟧(γ̂) ≜ ⌊bad⌋          if x ∉ γ
--     ⟦let x = e₁ in e₂⟧ ≜ ⟨b₁∨b₂,v̂₂⟩
--       where ⟨b₁,v̂₁⟩ = ⟦e₁⟧(γ̂)
--             ⟨b₂,v̂₂⟩ = ⟦e₂⟧(γ̂[x↦v̂₁])
--
-- HINTS:
-- - Define the interpreter by recursion over the input expression, starting
--   with a pattern match. In prior assignments I give this pattern match to
--   you, for this this one you need to figure it out on your own. Look at the
--   `src/Lang/L8/Data.hs` file to see what patterns are possible for the
--   `L8.Expr` datatype.
-- - Much of the math syntax is overloaded, such as `⌊_⌋`. Convince yourself
--   which helper function is the appropriate one to call, and make sure it
--   typechecks before moving on.
interpHat :: L8.Expr -> L8.EnvHat -> L8.AnswerHat
interpHat e env = error "TODO"

testE3 :: Test
testE3 = TestDir
  ( "E3"
  , "interpHat"
  , \ e -> interpHat e $ Map.fromList 
      [ ("true-or-false" , L8.ValueHat botInteger (Set.fromList [True,False]))
      , ("one-or-two" , L8.ValueHat (L8.RangeIH 1 2) Set.empty)
      , ("zero-to-ten" , L8.ValueHat (L8.RangeIH 0 10) Set.empty)
      ]
  , "tests/hw09/e3"
  , parseTest L8.pExpr L8.pAnswerHat
  )

-- ===== --
-- TESTS --
-- ===== --

main :: IO ()
main = runTests 
  [ testE1A
  , testE1B
  , testE1C
  , testE2
  , testE3
  ]

