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
  putStrLn (show [lmt| int * bool |])
  putStrLn (show [lmv| < fun x => y + 1 , {y = 2} > |])
  putStrLn (show [lma| <success> { loc 2 = 4 } , 1 |])
  putStrLn (show [lma| <error> { loc 2 = 4 } , "message" |])
  putStrLn (show [lma| <bad> |])
  putStrLn (show [lme| fun x : int => x + x |])
  putStrLn (show [lme| let p = (1*1, 2+2) in 
                       fst p * fst p + snd p |])
  putStrLn (show [lme| let tu2 = right false in 
                       let r2 = case tu2 {left x => x * x} {right x => if x then 1 else 2} in
                       r2 |])
