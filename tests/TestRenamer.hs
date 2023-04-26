{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TestRenamer (testRenamer, test, runPrint) where

import AnnForall (annotateForall)
import Control.Exception (
    ErrorCall (ErrorCall),
    Exception (displayException),
    SomeException (SomeException),
    evaluate,
    try,
 )
import Control.Exception.Extra (try_)
import Control.Monad (unless, (<=<))
import Control.Monad.Except (throwError)
import Data.Either.Extra (fromEither)
import DoStrings qualified as D
import GHC.Generics (Generic, Generic1)
import Grammar.Abs (Program (Program))
import Grammar.ErrM (Err, pattern Bad, pattern Ok)
import Grammar.Layout (resolveLayout)
import Grammar.Par (myLexer, pProgram)
import Grammar.Print (printTree)
import Renamer.Renamer (rename)
import System.IO.Error (catchIOError, tryIOError)
import Test.Hspec (
    anyErrorCall,
    anyException,
    describe,
    hspec,
    shouldBe,
    shouldNotSatisfy,
    shouldReturn,
    shouldSatisfy,
    shouldThrow,
    specify,
 )
import TypeChecker.ReportTEVar (reportTEVar)
import TypeChecker.TypeCheckerBidir (typecheck)
import TypeChecker.TypeCheckerIr qualified as T

-- FIXME tests sucks

test = hspec testRenamer

testRenamer = describe "Test Renamer" $ do
    rn_data1
    rn_data2
    rn_sig
    rn_bind1
    rn_bind2

rn_data1 = specify "Rename data type" . shouldSatisfyOk $
    D.do
        "data forall a. forall b. Either a b where"
        "    Left  : a -> Either a b"
        "    Right : b -> Either a b"

rn_data2 = specify "Rename data type forall in constructor " . shouldSatisfyOk $
    D.do
        "data forall a. forall b. Either a b where"
        "    Left  : forall c. c -> a -> Either a b"
        "    Right : b -> Either a b"

rn_sig =
    specify "Rename signature" $
        shouldSatisfyOk
            "f : forall a. forall b. a -> b -> (forall a. a -> a) -> a"

rn_bind1 =
    specify "Rename simple bind" $
        shouldSatisfyOk
            "f x = (\\y. let y2 = y + 1 in y2) (x + 1)"

rn_bind2 = specify "Rename bind with case" . shouldSatisfyOk $
    D.do
        "data forall a. List a where"
        "    Nil  : List a "
        "    Cons : a -> List a -> List a"

        "length : forall a. List a -> Int"
        "length list = case list of"
        "    Nil => 0"
        "    Cons x Nil => 1"
        "    Cons x (Cons y ys) => 2 + length ys"

runPrint = putStrLn . either show printTree . run $
    D.do
        "data forall a. List a where"
        "    Nil  : List a "
        "    Cons : a -> List a -> List a"

        "length : forall a. List a -> Int"
        "length list = case list of"
        "    Nil => 0"
        "    Cons x Nil => 1"
        "    Cons x (Cons y ys) => 2 + length ys"

shouldSatisfyOk s = run s `shouldSatisfy` ok

ok = \case
    Ok !_ -> True
    Bad !_ -> False

shouldBeErr s err = run s `shouldBe` Bad err

run = rename <=< run'
run' = pProgram . resolveLayout True . myLexer
