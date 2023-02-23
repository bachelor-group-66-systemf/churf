{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Grammar.Abs
import           System.Exit       (exitFailure)
import           Test.Hspec
import           TypeChecker.AlgoW

main :: IO ()
main = do
    print "RUNNING TESTS BROTHER"
    exitFailure
    -- hspec $ do
    --     describe "the algorithm W" $ do
    --         it "infers EInt as type Int" $ do
    --             fmap fst (run (inferExp (EInt 1))) `shouldBe` Right (TMono "Int")
    --         it "throws an exception if a variable is inferred with an empty env" $ do
    --             run (inferExp (EId "x")) `shouldBe` Left "Unbound variable: x"
    --         it "throws an exception if the annotated type does not match the inferred type" $ do
    --             fmap fst (run (inferExp (EAnn (EInt 3) (TPol "a")))) `shouldBe` Right (TMono "bad")
