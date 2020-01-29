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

module Lang.Trees.Util where

import UVMHS

import Lang.Trees.Data

import qualified Prelude as HS
import qualified Language.Haskell.TH.Syntax as QQ
import qualified Language.Haskell.TH.Quote as QQ

makePrettySum ''Tree

deriving instance QQ.Lift Tree

instance Pretty HS.Int where pretty = pretty ∘ (HS.fromIntegral ∷ HS.Int → HS.Integer)

lexer ∷ Lexer CharClass ℂ TokenClassBasic ℕ64 TokenBasic
lexer = lexerBasic puns kws prim ops
  where
    puns = list ["<",">",":",","]
    kws = list ["TEST","EXPECTED"]
    prim = list ["L"]
    ops = list []

pTree ∷ CParser TokenBasic Tree
pTree = cpNewContext "tree" $ concat
  [ do cpSyntax "L"
       return Leaf
  , do cpSyntax "<" 
       i ← HS.fromIntegral ^$ cpInteger 
       cpSyntax ":"
       t₁ ← pTree
       cpSyntax ","
       t₂ ← pTree
       cpSyntax ">"
       return $ Node i t₁ t₂
  ]

pTTest ∷ CParser TokenBasic (Tree,Tree)
pTTest = cpNewContext "test" $ concat
  [ do cpSyntax "TEST"
       t ← pTree
       cpSyntax "EXPECTED"
       t' ← pTree
       return (t,t')
  ]

pRTest ∷ CParser TokenBasic (Tree,HS.Int)
pRTest = cpNewContext "test" $ concat
  [ do cpSyntax "TEST"
       t ← pTree
       cpSyntax "EXPECTED"
       i ← HS.fromIntegral ^$ cpInteger
       return (t,i)
  ]

parseTree ∷ 𝕊 → IO Tree
parseTree = parseIO pTree *∘ tokenizeIO lexer ∘ tokens

parseTTest ∷ 𝕊 → IO (Tree,Tree)
parseTTest = parseIO pTTest *∘ tokenizeIO lexer ∘ tokens

parseRTest ∷ 𝕊 → IO (Tree,HS.Int)
parseRTest = parseIO pRTest *∘ tokenizeIO lexer ∘ tokens

quoteTree ∷ HS.String → QQ.Q QQ.Exp
quoteTree cs = do
  e ← QQ.runIO $ parseTree $ string cs
  [| e |]

tree ∷ QQ.QuasiQuoter
tree = QQ.QuasiQuoter quoteTree (const $ HS.fail $ chars "quote pattern - I can't even") 
                                (const $ HS.fail $ chars "quote type - I can't even") 
                                (const $ HS.fail $ chars "quote dec - I can't even")
