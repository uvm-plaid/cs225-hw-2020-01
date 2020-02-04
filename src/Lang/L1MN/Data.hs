module Lang.L1MN.Data where

data Expr = 
    IntE Integer
  | PlusE Expr Expr 
  | TimesE Expr Expr
  | MinusE Expr Expr
  | NegE Expr
  deriving (Eq,Ord,Show)

data Value = 
  IntV Integer
  deriving (Eq,Ord,Show)

data Answer = 
  ValueA Value
  deriving (Eq,Ord,Show)

