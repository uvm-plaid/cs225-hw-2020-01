module Lang.L4.Data where

import Data.Map (Map)

data Expr = 
    IntE Integer
  | PlusE Expr Expr 
  | TimesE Expr Expr
  | BoolE Bool
  | IfE Expr Expr Expr
  | VarE String
  | LetE String Expr Expr
  -- NEW
  | CallE String [Expr]
  deriving (Eq,Ord,Show)

-- NEW
data Command =
    DefC String [String] Expr
  deriving (Eq,Ord,Show)

-- NEW
data Program =
    Program [Command] Expr
  deriving (Eq,Ord,Show)

data Value = 
    IntV Integer
  | BoolV Bool
  deriving (Eq,Ord,Show)

data Answer = 
    ValueA Value
  | BadA
  deriving (Eq,Ord,Show)

type Env = Map String Value

-- NEW
type FEnv = Map String ([String],Expr)
