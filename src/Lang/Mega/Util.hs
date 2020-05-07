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

module Lang.Mega.Util where

import UVMHS

import Util.Lex

import Lang.Mega.Data

import qualified Prelude as HS
import qualified Language.Haskell.TH.Syntax as QQ
import qualified Language.Haskell.TH.Quote as QQ

import Data.Map (Map) 
import qualified Data.Map as Map

import Data.Set (Set) 
import qualified Data.Set as Set

makePrettySum ''Expr
makePrettySum ''Type
makePrettySum ''Value
makePrettySum ''Answer

instance (QQ.Lift a) ⇒ QQ.Lift (Set a) where
  lift xs = 
    let xs' = Set.toList xs
    in [| Set.fromList xs' |]

instance (QQ.Lift k,QQ.Lift v) ⇒ QQ.Lift (Map k v) where
  lift kvs =
    let kvs' = Map.toList kvs
    in [| Map.fromList kvs' |]

deriving instance QQ.Lift Expr
deriving instance QQ.Lift Type
deriving instance QQ.Lift Value
deriving instance QQ.Lift Answer

level_LET        = 1
level_ASSIGN     = 2
level_TUPLE      = 5
level_ARROW      = 10
level_PLUS       = 11
level_TIMES      = 12
level_APP        = 21
level_UNBOX      = 22
level_ACCESS     = 23


pExpr ∷ CParser TokenBasic Expr
pExpr = cpNewContext "expression" $ mixfix $ concat
  -- [ mixTerminal $ do cpSyntax "(" ; e ← pExpr ; cpSyntax ")" ; return e
  [ mixTerminal $ do i ← cpInteger ; return $ IntE i
  , mixInfixL (𝕟64 level_PLUS) $ do cpSyntax "+" ; return PlusE
  , mixInfixL (𝕟64 level_PLUS) $ do cpSyntax "*" ; return TimesE
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
      xτ ← tries
        [ do x ← pVar
             τO ← tohs ^$ cpOptional $ do
               cpSyntax ":"
               pType 
             return $ Inl (x :* τO)
        , do cpSyntax "["
             x₁ ← pVar
             τ₁₂O ← tohs ^$ cpOptional $ do
               cpSyntax ":" 
               τ₁ ← pType
               cpSyntax "->"
               τ₂ ← pType
               return $ τ₁ :* τ₂
             cpSyntax "]"
             x₂ ← pVar
             return $ Inr (x₁ :* τ₁₂O :* x₂)
        ]
      cpSyntax "=>"
      return $ case xτ of
        Inl (x :* τO) → FunE x τO
        Inr (x₁ :* τ₁₂O :* x₂) → RecFunE x₁ τ₁₂O x₂
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
  , mixPrefix (𝕟64 level_LET) $ do
      cpSyntax "mut"
      e ← pExpr
      cpSyntax "in"
      return $ LetE (chars "_") e
  , mixTerminal $ do
      cpSyntax "class"
      cpSyntax "fields"
      xτOs ← cpMany $ do
        x ← pVar
        τO ← tohs ^$ cpOptional $ do
          cpSyntax ":"
          pType
        return $ x :* τO
      xes ← cpMany $ do
        cpSyntax "method"
        x ← pVar
        cpSyntax "=>"
        e ← pExpr
        return $ x :* e
      cpSyntax "end"
      return $ ClassE (tohs xτOs) $ tohs xes
  , mixTerminal $ do
      cpSyntax "new"
      e₁ ← pExpr
      cpSyntax "{"
      xes ← cpManySepBy (cpSyntax ",") $ do
        x ← pVar
        cpSyntax "="
        e ← pExpr
        return $ x :* e
      cpSyntax "}"
      return $ NewE e₁ $ tohs xes
  , mixPostfix (𝕟64 level_ACCESS) $ do
      cpSyntax "."
      x ← pVar
      return $ \ e → AccessE e x
  , mixTerminal $ do 
      cpSyntax "("
      e ← pExpr
      e' ← tries
        [ do cpSyntax ","
             es ← cpOneOrMoreSepBy (cpSyntax ",") pExpr
             return $ foldOnFrom es e $ \ eᵢ eₐ → PairE eₐ eᵢ
        , do return e
        ] 
      cpSyntax ")"
      return e'
  , mixPrefix (𝕟64 level_APP) $ do cpSyntax "fst" ; return FstE
  , mixPrefix (𝕟64 level_APP) $ do cpSyntax "snd" ; return SndE
  , mixPrefix (𝕟64 level_APP) $ do 
      cpSyntax "left" 
      τO ← tohs ^$ cpOptional pType
      return $ LeftE τO
  , mixPrefix (𝕟64 level_APP) $ do 
      cpSyntax "right" 
      τO ← tohs ^$ cpOptional pType
      return $ RightE τO
  , mixTerminal $ do 
      cpSyntax "case" 
      e₁ ← pExpr
      cpSyntax "{"
      cpSyntax "left"
      x₁ ← pVar
      cpSyntax "=>"
      e₂ ← pExpr
      cpSyntax "}"
      cpSyntax "{"
      cpSyntax "left"
      x₂ ← pVar
      cpSyntax "=>"
      e₃ ← pExpr
      cpSyntax "}"
      return $ CaseE e₁ x₁ e₂ x₂ e₃
  , mixPrefix (𝕟64 level_LET) $ do
      cpSyntax "while"
      e ← pExpr
      cpSyntax "do"
      return $ WhileE e
  , mixTerminal $ do s ← cpString ; return $ StringE $ chars s
  , mixPrefix (𝕟64 level_APP) $ do cpSyntax "throw" ; return ThrowE
  , mixPrefix (𝕟64 level_LET) $ do
      cpSyntax "try"
      e ← pExpr
      cpSyntax "catch"
      x ← pVar
      cpSyntax "=>"
      return $ TryE e x
  ]

pType ∷ CParser TokenBasic Type
pType = cpNewContext "type" $ mixfix $ concat
  [ mixTerminal $ do cpSyntax "(" ; τ ← pType ; cpSyntax ")" ; return τ
  , mixTerminal $ do cpSyntax "int" ; return IntT
  , mixTerminal $ do cpSyntax "bool" ; return IntT
  , mixInfix (𝕟64 level_ARROW) $ do cpSyntax "->" ; return FunT
  , mixTerminal $ do
      cpSyntax "class"
      cpSyntax "fields"
      xτs₁ ← cpMany $ do
        x ← pVar
        cpSyntax ":"
        τ ← pType
        return $ x :* τ
      cpSyntax "methods"
      xτs₂ ← cpMany $ do
        x ← pVar
        cpSyntax ":"
        τ ← pType
        return $ x :* τ
      cpSyntax "end"
      return $ ClassT (tohs xτs₁) $ tohs xτs₂
  , mixTerminal $ do
      cpSyntax "object"
      cpSyntax "fields"
      xτs₁ ← cpMany $ do
        x ← pVar
        cpSyntax ":"
        τ ← pType
        return $ x :* τ
      cpSyntax "methods"
      xτs₂ ← cpMany $ do
        x ← pVar
        cpSyntax ":"
        τ ← pType
        return $ x :* τ
      cpSyntax "end"
      return $ ObjectT (tohs xτs₁) $ tohs xτs₂
  , mixPrefix (𝕟64 level_APP) $ do cpSyntax "box" ; return BoxT
  , mixInfixL (𝕟64 level_TIMES) $ do cpSyntax "*" ; return PairT
  , mixInfixL (𝕟64 level_PLUS) $ do cpSyntax "+" ; return TUnionT
  , mixTerminal $ do cpSyntax "string" ; return StringT
  ]

pLoc ∷ CParser TokenBasic ℤ
pLoc = do cpSyntax "loc" ; cpInteger

pValue ∷ CParser TokenBasic Value
pValue = cpNewContext "value" $ mixfix $ concat
  [ mixTerminal $ do i ← cpInteger ; return $ IntV i
  , mixTerminal $ do b ← pBool ; return $ BoolV b
  , mixTerminal $ do 
      cpSyntax "<"
      cpSyntax "fun"
      xO ← cpOptional $ do
        cpSyntax "["
        x ← pVar
        cpSyntax "]"
        return x
      x ← pVar
      cpSyntax "=>"
      e ← pExpr
      cpSyntax ","
      γ ← pEnv
      cpSyntax ">"
      return $ case xO of
        None → CloV x e γ
        Some x' → RecCloV x' x e γ
  , mixTerminal $ do 
      ℓ ← pLoc
      return $ LocV ℓ
  , mixTerminal $ do 
      cpSyntax "<"
      cpSyntax "class"
      cpSyntax "fields"
      xs ← cpMany pVar
      xes ← cpMany $ do
        cpSyntax "method"
        x ← pVar
        cpSyntax "=>"
        e ← pExpr
        return $ x :* e
      cpSyntax "end"
      cpSyntax ","
      γ ← pEnv
      cpSyntax ">"
      return $ ClassV (tohs xs) (tohs xes) γ
  , mixTerminal $ do 
      cpSyntax "<"
      cpSyntax "object"
      xis ← pMap pVar pInt
      xes ← pMap pVar pExpr
      cpSyntax ","
      γ ← pEnv
      cpSyntax ">"
      return $ ObjectV xis xes γ
  , mixTerminal $ do 
      cpSyntax "("
      v ← pValue
      v' ← tries
        [ do cpSyntax ","
             vs ← cpOneOrMoreSepBy (cpSyntax ",") pValue
             return $ foldOnFrom vs v $ \ vᵢ vₐ → PairV vₐ vᵢ
        , do return v
        ] 
      cpSyntax ")"
      return v'
  , mixPrefix (𝕟64 level_APP) $ do cpSyntax "left" ; return LeftV
  , mixPrefix (𝕟64 level_APP) $ do cpSyntax "right" ; return RightV
  , mixTerminal $ do s ← cpString ; return $ StringV $ chars s
  ]

pAnswer ∷ CParser TokenBasic Answer
pAnswer = tries
  [ do cpSyntax "<success>"
       σ ← pSto
       cpSyntax ","
       v ← pValue
       return $ SuccessA σ v
  , do cpSyntax "<error>"
       σ ← pSto
       cpSyntax ","
       s ← cpString
       return $ ErrorA σ $ chars s
  , do cpSyntax "<bad>"
       return BadA
  ]

pEnv ∷ CParser TokenBasic Env
pEnv = pMap pVar pValue

pSto ∷ CParser TokenBasic Store
pSto = pMap pLoc pValue

parseExpr ∷ 𝕊 → IO Expr
parseExpr = parseIO pExpr *∘ tokenizeIO lexer ∘ tokens

quoteExpr ∷ HS.String → QQ.Q QQ.Exp
quoteExpr cs = do
  e ← QQ.runIO $ parseExpr $ string cs
  [| e |]

lme ∷ QQ.QuasiQuoter
lme = QQ.QuasiQuoter (\ cs → do e ← QQ.runIO $ lexAndParseIO pExpr $ string cs ; [| e |])
                     (const $ HS.fail $ chars "quote pattern - I can't even") 
                     (const $ HS.fail $ chars "quote type - I can't even") 
                     (const $ HS.fail $ chars "quote dec - I can't even")

lmt ∷ QQ.QuasiQuoter
lmt = QQ.QuasiQuoter (\ cs → do τ ← QQ.runIO $ lexAndParseIO pType $ string cs ; [| τ |])
                     (const $ HS.fail $ chars "quote pattern - I can't even") 
                     (const $ HS.fail $ chars "quote type - I can't even") 
                     (const $ HS.fail $ chars "quote dec - I can't even")

lmv ∷ QQ.QuasiQuoter
lmv = QQ.QuasiQuoter (\ cs → do v ← QQ.runIO $ lexAndParseIO pValue $ string cs ; [| v |])
                     (const $ HS.fail $ chars "quote pattern - I can't even") 
                     (const $ HS.fail $ chars "quote type - I can't even") 
                     (const $ HS.fail $ chars "quote dec - I can't even")

lma ∷ QQ.QuasiQuoter
lma = QQ.QuasiQuoter (\ cs → do a ← QQ.runIO $ lexAndParseIO pAnswer $ string cs ; [| a |])
                     (const $ HS.fail $ chars "quote pattern - I can't even") 
                     (const $ HS.fail $ chars "quote type - I can't even") 
                     (const $ HS.fail $ chars "quote dec - I can't even")

lmg ∷ QQ.QuasiQuoter
lmg = QQ.QuasiQuoter (\ cs → do γ ← QQ.runIO $ lexAndParseIO pEnv $ string cs ; [| γ |])
                     (const $ HS.fail $ chars "quote pattern - I can't even") 
                     (const $ HS.fail $ chars "quote type - I can't even") 
                     (const $ HS.fail $ chars "quote dec - I can't even")

lms ∷ QQ.QuasiQuoter
lms = QQ.QuasiQuoter (\ cs → do σ ← QQ.runIO $ lexAndParseIO pSto $ string cs ; [| σ |])
                     (const $ HS.fail $ chars "quote pattern - I can't even") 
                     (const $ HS.fail $ chars "quote type - I can't even") 
                     (const $ HS.fail $ chars "quote dec - I can't even")
