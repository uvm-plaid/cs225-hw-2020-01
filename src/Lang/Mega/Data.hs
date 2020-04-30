module Lang.Mega.Data where

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
  | FunE String (Maybe Type) Expr
  | AppE Expr Expr
  | BoxE Expr
  | UnboxE Expr
  | AssignE Expr Expr
  | ClassE [(String, Maybe Type)] [(String, Expr)]
  | NewE Expr [(String, Expr)]
  | AccessE Expr String
  -- New in final projects
  | PairE Expr Expr
  | FstE Expr
  | SndE Expr
  | LeftE (Maybe Type) Expr
  | RightE (Maybe Type) Expr
  | CaseE Expr String Expr String Expr
  | WhileE Expr Expr
  | RecFunE String (Maybe (Type, Type)) String Expr
  | StringE String
  | ThrowE Expr
  | TryE Expr String Expr
  deriving (Eq,Ord,Show)

-----------
-- TYPES --
-----------

data Type =
    IntT
  | BoolT
  | FunT Type Type
  | ClassT [(String, Type)] [(String, Type)]
  | ObjectT [(String, Type)] [(String, Type)]
  | BoxT Type
  | PairT Type Type
  | TUnionT Type Type
  | StringT
  deriving (Eq,Ord,Show)

---------------
-- SEMANTICS --
---------------

data Value = 
    IntV Integer
  | BoolV Bool
  | CloV String Expr Env
  | LocV Integer
  | ClassV [String] [(String, Expr)] Env
  | ObjectV (Map String Integer) (Map String Expr) Env
  -- New in final projects
  | PairV Value Value
  | LeftV Value
  | RightV Value
  | RecCloV String String Expr Env
  | StringV String
  deriving (Eq,Ord,Show)

type Env = Map String Value

type Store = Map Integer Value

data Answer = 
    SuccessA Store Value
  | ErrorA Store String
  | BadA
  deriving (Eq,Ord,Show)
