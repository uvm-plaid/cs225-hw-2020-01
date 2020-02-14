module Lang.L3.Data where

import Data.Map (Map)

data Expr = 
    IntE Integer
  | PlusE Expr Expr 
  | TimesE Expr Expr
  | BoolE Bool
  -- NEW
  | IfE Expr Expr Expr
  | VarE String
  | LetE String Expr Expr
  deriving (Eq,Ord,Show)

data Value = 
    IntV Integer
  | BoolV Bool
  deriving (Eq,Ord,Show)

-- NEW
type Env = Map String Value

data Answer = 
    ValueA Value
  | BadA
  deriving (Eq,Ord,Show)

