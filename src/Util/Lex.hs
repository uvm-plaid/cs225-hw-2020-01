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

module Lang.Lex where

import UVMHS

import qualified Data.Set as Set

lexer ∷ Lexer CharClass ℂ TokenClassBasic ℕ64 TokenBasic
lexer = lexerBasic puns kws prim ops
  where
    puns = list ["(",")","{","}",".",",",";",":","=","->"]
    kws = list ["TEST","EXPECTED","AND","let","in","object"]
    prim = list ["true","false","bad"]
    ops = list ["+","-","*","/","<",">","<=",">=","==","/=","||","&&","!"]

pBool ∷ CParser TokenBasic 𝔹
pBool = concat
  [ do cpSyntax "true" ; return True
  , do cpSyntax "false" ; return False
  ]

pInt ∷ CParser TokenBasic ℤ
pInt = cpInteger

pSet ∷ (Ord a) ⇒ CParser TokenBasic a → CParser TokenBasic (Set.Set a)
pSet pX = cpNewContext "set" $ do
  cpSyntax "{"
  xs ← cpManySepBy (cpSyntax ",") pX
  cpSyntax "}"
  return $ Set.fromList $ lazyList xs

instance (Pretty a,Ord a) ⇒ Pretty (Set.Set a) where pretty = pretty ∘ pow ∘ Set.toList

pPair ∷ CParser TokenBasic a → CParser TokenBasic b → CParser TokenBasic (a,b)
pPair pX pY = cpNewContext "pair" $ do
  x ← pX
  cpSyntax "AND"
  y ← pY
  return (x,y)

pTest ∷ CParser TokenBasic a → CParser TokenBasic b → CParser TokenBasic (a,b)
pTest pA pB = cpNewContext "test" $ concat
  [ do cpSyntax "TEST"
       e ← pA
       cpSyntax "EXPECTED"
       a ← pB
       return (e,a)
  ]

parseTest ∷ (Pretty a,Pretty b) ⇒ CParser TokenBasic a → CParser TokenBasic b → 𝕊 → IO (a,b)
parseTest pA pB = parseIO (pTest pA pB) *∘ tokenizeIO lexer ∘ tokens
