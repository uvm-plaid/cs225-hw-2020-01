module Lang.L6.Data where

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
