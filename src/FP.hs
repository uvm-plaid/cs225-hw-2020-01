{-# LANGUAGE QuasiQuotes #-}
module FP where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Util.Lex
import Util.Testing

import Lang.Mega as LM

import Control.Monad

test1 :: Test
test1 = TestDir
  ( "T1"                 -- e.g., "T1"
  , "identity function"  -- e.g., "interp"
  , id                   -- the function, e.g., (\ e -> interp e Map.empty Map.empty)
  , "tests/fp"           -- the directory where tests live, e.g., "tests/fp/t1"
  , parseTest LM.pExpr LM.pExpr
  )

test2 :: Test
test2 = Test1
  ( "T2"
  , "identity function"
  , id
  , [ -- test input
      ( [lme| let p = (1,2) in fst p |] 
      -- expeced output
      , [lme| let p = (1,2) in fst p |]
      )
    ]
  )

main :: IO ()
main = do
  putStrLn "TESTS"
  runTests 
    [ test1
    , test2
    ]
  putStrLn "EXAMPLE"
  putStrLn (show [lme| let p = (1,2) in fst p |])
