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

module Lang.L5.Util where

import UVMHS

import Util.Lex

import Lang.L5.Data

import qualified Prelude as HS
import qualified Language.Haskell.TH.Syntax as QQ
import qualified Language.Haskell.TH.Quote as QQ

import qualified Data.Map as Map

makePrettySum ''Expr
makePrettySum ''ValueE
makePrettySum ''AnswerE
makePrettySum ''ValueS
makePrettySum ''AnswerS

deriving instance QQ.Lift Expr

pExpr ∷ CParser TokenBasic Expr
pExpr = cpNewContext "expression" $ mixfix $ concat
  [ mixTerminal $ do cpSyntax "(" ; e ← pExpr ; cpSyntax ")" ; return e
  , mixTerminal $ do i ← cpInteger ; return $ IntE i
  , mixInfixL (𝕟64 2) $ do cpSyntax "+" ; return PlusE
  , mixInfixL (𝕟64 3) $ do cpSyntax "*" ; return TimesE
  , mixTerminal $ do b ← pBool ; return $ BoolE b
  , mixPrefix (𝕟64 1) $ do
      cpSyntax "if"
      e₁ ← pExpr
      cpSyntax "then"
      e₂ ← pExpr
      cpSyntax "else"
      return $ IfE e₁ e₂
  , mixTerminal $ do x ← pVar ; return $ VarE x
  , mixPrefix (𝕟64 1) $ do
      cpSyntax "let"
      x ← pVar
      cpSyntax "="
      e ← pExpr
      cpSyntax "in"
      return $ LetE x e
  , mixPrefix (𝕟64 1) $ do
      cpSyntax "fun"
      x ← pVar
      cpSyntax "=>"
      return $ FunE x
  , mixInfixL (𝕟64 10) $ return AppE
  ]

pValueE ∷ CParser TokenBasic ValueE
pValueE = cpNewContext "value" $ concat
  [ do i ← cpInteger ; return $ IntEV i
  , do b ← pBool ; return $ BoolEV b
  , do cpSyntax "("
       cpSyntax "fun"
       x ← pVar
       cpSyntax "=>"
       e ← pExpr
       cpSyntax ";"
       γ ← pEnvE
       cpSyntax ")"
       return $ CloEV x e γ
  ]

pAnswerE ∷ CParser TokenBasic AnswerE
pAnswerE = cpNewContext "answer" $ concat
  [ do v ← pValueE ; return $ ValueEA v
  , do cpSyntax "bad" ; return BadEA
  ]

pEnvE ∷ CParser TokenBasic EnvE
pEnvE = pMap pVar pValueE

pValueS ∷ CParser TokenBasic ValueS
pValueS = cpNewContext "value" $ concat
  [ do i ← cpInteger ; return $ IntSV i
  , do b ← pBool ; return $ BoolSV b
  , do cpSyntax "fun"
       x ← pVar
       cpSyntax "=>"
       e ← pExpr
       return $ FunSV x e
  ]

pAnswerS ∷ CParser TokenBasic AnswerS
pAnswerS = cpNewContext "answer" $ concat
  [ do v ← pValueS ; return $ ValueSA v
  , do cpSyntax "bad" ; return BadSA
  ]

pEnvS ∷ CParser TokenBasic EnvS
pEnvS = pMap pVar pValueS

parseExpr ∷ 𝕊 → IO Expr
parseExpr = parseIO pExpr *∘ tokenizeIO lexer ∘ tokens

quoteExpr ∷ HS.String → QQ.Q QQ.Exp
quoteExpr cs = do
  e ← QQ.runIO $ parseExpr $ string cs
  [| e |]

l4 ∷ QQ.QuasiQuoter
l4 = QQ.QuasiQuoter quoteExpr (const $ HS.fail $ chars "quote pattern - I can't even") 
                              (const $ HS.fail $ chars "quote type - I can't even") 
                              (const $ HS.fail $ chars "quote dec - I can't even")
