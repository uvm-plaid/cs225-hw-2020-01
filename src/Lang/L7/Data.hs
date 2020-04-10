module Lang.L7.Data where

import Data.Map (Map)
import Data.Set (Set)

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
  | BoxE Expr
  | UnboxE Expr
  | AssignE Expr Expr
  -- NEW
  | ClassE [String] [(String, Expr)]
  | NewE Expr [(String, Expr)]
  | AccessE Expr String
  deriving (Eq,Ord,Show)

---------------
-- SEMANTICS --
---------------

data Value = 
    IntV Integer
  | BoolV Bool
  | CloV String Expr Env
  | LocV Integer
  -- NEW
  | ClassV [String] [(String, Expr)] Env
  | ObjectV (Map String Integer) (Map String Expr) Env
  deriving (Eq,Ord,Show)

type Env = Map String Value

type Store = Map Integer Value

type Answer = Maybe (Store, Value)
