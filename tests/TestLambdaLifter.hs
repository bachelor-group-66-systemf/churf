{-# LANGUAGE PatternSynonyms   #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo       #-}

module TestLambdaLifter where

import           Test.Hspec

import           AnnForall                    (annotateForall)
import           Control.Monad                ((<=<))
import           Control.Monad.Error.Class    (liftEither)
import           Control.Monad.Extra          (eitherM)
import           Grammar.ErrM                 (Err, pattern Bad, pattern Ok)
import           Grammar.Layout               (resolveLayout)
import           Grammar.Par                  (myLexer, pProgram)
import           Grammar.Print                (printTree)
import           LambdaLifter
import           Renamer.Renamer              (rename)
import           ReportForall                 (reportForall)
import           TypeChecker.RemoveForall     (removeForall)
import           TypeChecker.ReportTEVar      (reportTEVar)
import           TypeChecker.TypeChecker      (TypeChecker (Bi))
import           TypeChecker.TypeCheckerBidir (typecheck)
import           TypeChecker.TypeCheckerIr


test = hspec testLambdaLifter

testLambdaLifter = describe "Test Lambda Lifter" $ do
    undefined
--  frees_exp1

-- frees_exp1 = specify "Free variables 1" $
--     freeVarsExp [] (EAbs "x" (EVar "x", TVar' "a"), TVar' "a")
--     `shouldBe` answer
--   where
--     answer = Ann { frees = []
--                  , term = (AAbs (Ident "x") (Ann { frees = [Ident "x"]
--                                                  , term = (AVar (Ident "x"),TVar (MkTVar (Ident "a")))
--                                                  }
--                                             ),TVar (MkTVar (Ident "a")))
--                  }


abs_1 = undefined
  where
    input = unlines [ "data List (a) where"
                    , "    Nil  : List (a)"
                    , "    Cons : a -> List (a) -> List (a)"
                    , "map : (a -> b) -> List (a) -> List (b)"
                    , "add : Int -> Int -> Int"

                    , "f : List (Int)"
                    , "f = (\\x.\\ys. map (\\y. add y x) ys) 4 (Cons 1 (Cons 2 Nil))"
                    ]



runPrintFree = print $ freeVarsExp [] (EAbs "x" (EVar "x", TVar' "a"), TVar' "a")

runAbstract = either putStrLn (putStrLn . printTree) (runAbs s2)
  where
    s = unlines [ "add : Int -> Int -> Int"
                , "f : Int -> Int -> Int"
                , "f x y = add x y"
                , "f = \\x. (\\y. add x y)"
                ]

    s2 = unlines [ "data List (a) where"
                 , "    Nil  : List (a)"
                 , "    Cons : a -> List (a) -> List (a)"
                 , "map : (a -> b) -> List (a) -> List (b)"
                 , "add : Int -> Int -> Int"

                 , "f : List (Int)"
                 , "f = (\\x.\\ys. map (\\y. add y x) ys) 4 (Cons 1 (Cons 2 Nil))"
                 ]


runCollect = either putStrLn (putStrLn . printTree) (run s)
  where
    s = unlines [ "data List (a) where"
                , "    Nil  : List (a)"
                , "    Cons : a -> List (a) -> List (a)"
                , "add : Int -> Int -> Int"
                , "map : (a -> b) -> List (a) -> List (b)"
                , "map f xs = case xs of"
                , "    Nil       => Nil"
                , "    Cons x xs => Cons (f x) (map f xs)"

                , "f : List (Int)"
                , "f = (\\x.\\ys. map (\\y. add y x) ys) 4 (Cons 1 (Cons 2 Nil))"
                ]


run = fmap collectScs . runAbs

runAbs s = do
  Program ds <- run' s
  pure $ (abstract . freeVars) [b | DBind b <- ds]


run' =   fmap removeForall
      . reportTEVar
    <=< typecheck
    <=< run''

run'' s = do
    p <- (pProgram . resolveLayout True . myLexer) s
    reportForall Bi p
    (rename <=< annotateForall) p



