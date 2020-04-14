{-# LANGUAGE 
    ConstraintKinds
   ,DataKinds
   ,ExplicitNamespaces
   ,FlexibleContexts
   ,FlexibleInstances
   ,FunctionalDependencies
   ,GADTs
   ,GeneralizedNewtypeDeriving
   ,InstanceSigs
   ,KindSignatures
   ,LambdaCase
   ,MonadComprehensions
   ,MultiParamTypeClasses
   ,NoImplicitPrelude
   ,OverloadedStrings
   ,PartialTypeSignatures
   ,PatternSynonyms
   ,PolyKinds
   ,QuantifiedConstraints
   ,RankNTypes
   ,RebindableSyntax
   ,ScopedTypeVariables
   ,StandaloneDeriving
   ,Strict
   ,StrictData
   ,TemplateHaskell
   ,TypeApplications
   ,TypeFamilies
   ,TypeOperators
   ,UndecidableInstances
   ,UndecidableSuperClasses
   ,UnicodeSyntax
   ,ViewPatterns 
   ,DeriveLift #-}

module Lang.L6.Util where

import UVMHS

import Util.Lex

import Lang.L6.Data

import qualified Prelude as HS
import qualified Language.Haskell.TH.Syntax as QQ
import qualified Language.Haskell.TH.Quote as QQ

import qualified Data.Map as Map

makePrettySum ''Expr
makePrettySum ''Value

deriving instance QQ.Lift Expr

level_LET        = 1
level_ASSIGN     = 2
level_PLUS       = 11
level_TIMES      = 12
level_APP        = 21
level_UNBOX      = 22

pExpr ∷ CParser TokenBasic Expr
pExpr = cpNewContext "expression" $ mixfix $ concat
  [ mixTerminal $ do cpSyntax "(" ; e ← pExpr ; cpSyntax ")" ; return e
  , mixTerminal $ do i ← cpInteger ; return $ IntE i
  , mixInfixL (𝕟64 level_PLUS) $ do cpSyntax "+" ; return PlusE
  , mixInfixL (𝕟64 level_TIMES) $ do cpSyntax "*" ; return TimesE
  , mixTerminal $ do b ← pBool ; return $ BoolE b
  , mixPrefix (𝕟64 level_LET) $ do
      cpSyntax "if"
      e₁ ← pExpr
      cpSyntax "then"
      e₂ ← pExpr
      cpSyntax "else"
      return $ IfE e₁ e₂
  , mixTerminal $ do x ← pVar ; return $ VarE x
  , mixPrefix (𝕟64 level_LET) $ do
      cpSyntax "let"
      x ← pVar
      cpSyntax "="
      e ← pExpr
      cpSyntax "in"
      return $ LetE x e
  , mixPrefix (𝕟64 level_LET) $ do
      cpSyntax "fun"
      x ← pVar
      cpSyntax "=>"
      return $ FunE x
  , mixInfixL (𝕟64 level_APP) $ return AppE
  , mixPrefix (𝕟64 level_APP) $ do
      cpSyntax "box"
      return $ BoxE
  , mixPrefix (𝕟64 level_UNBOX) $ do
      cpSyntax "!"
      return $ UnboxE
  , mixInfixR (𝕟64 level_ASSIGN) $ do
      cpSyntax "<-"
      return $ AssignE
  , mixInfixR (𝕟64 level_LET) $ do
      cpSyntax ";"
      return $ \ e₁ e₂ → LetE (chars "_") e₁ e₂
  ]

pValue ∷ CParser TokenBasic Value
pValue = cpNewContext "value" $ concat
  [ do i ← cpInteger ; return $ IntV i
  , do b ← pBool ; return $ BoolV b
  , do cpSyntax "("
       cpSyntax "fun"
       x ← pVar
       cpSyntax "=>"
       e ← pExpr
       cpSyntax ")"
       cpSyntax ","
       γ ← pEnv
       return $ CloV x e γ
  , do cpSyntax "loc"
       i ← cpInteger 
       return $ LocV i
  ]

pAnswer ∷ CParser TokenBasic Answer
pAnswer = pMaybe (pPair pStore pValue)

pEnv ∷ CParser TokenBasic Env
pEnv = pMap pVar pValue

pStore ∷ CParser TokenBasic Store
pStore = pMap cpInteger pValue

parseExpr ∷ 𝕊 → IO Expr
parseExpr = parseIO pExpr *∘ tokenizeIO lexer ∘ tokens

quoteExpr ∷ HS.String → QQ.Q QQ.Exp
quoteExpr cs = do
  e ← QQ.runIO $ parseExpr $ string cs
  [| e |]

l6 ∷ QQ.QuasiQuoter
l6 = QQ.QuasiQuoter quoteExpr (const $ HS.fail $ chars "quote pattern - I can't even") 
                              (const $ HS.fail $ chars "quote type - I can't even") 
                              (const $ HS.fail $ chars "quote dec - I can't even")
