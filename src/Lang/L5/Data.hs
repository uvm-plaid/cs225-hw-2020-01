module Lang.L5.Data where

import Data.Map (Map)

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
  -- NEW
  | FunE String Expr
  | AppE Expr Expr
  deriving (Eq,Ord,Show)

---------------------------
-- ENVIRONMENT SEMANTICS --
---------------------------

data ValueE = 
    IntEV Integer
  | BoolEV Bool
  -- NEW
  | CloEV String Expr EnvE
  deriving (Eq,Ord,Show)

data AnswerE = 
    ValueEA ValueE
  | BadEA
  deriving (Eq,Ord,Show)

type EnvE = Map String ValueE

----------------------------
-- SUBSTITUTION SEMANTICS --
----------------------------

data ValueS =
    IntSV Integer
  | BoolSV Bool
  | FunSV String Expr
  deriving (Eq,Ord,Show)

data AnswerS =
    ValueSA ValueS
  | BadSA
  deriving (Eq,Ord,Show)

type EnvS = Map String ValueS
