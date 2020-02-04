module Lang.L2C.Data where

data Expr = 
    IntE Integer
  | PlusE Expr Expr 
  | TimesE Expr Expr
  -- NEW
  | BoolE Bool
  | OrE Expr Expr
  -- | AndE Expr Expr
  | NegE Expr
  | EqE Expr Expr
  deriving (Eq,Ord,Show)

data Value = 
    IntV Integer
  -- NEW
  | BoolV Bool
  deriving (Eq,Ord,Show)

data Answer = 
    ValueA Value
  -- NEW
  | BadA
  deriving (Eq,Ord,Show)

