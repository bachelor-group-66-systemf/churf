{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TestAnnForall (testAnnForall, test) where

import AnnForall (annotateForall)
import Control.Monad ((<=<))
import DoStrings qualified as D
import Grammar.ErrM (Err, pattern Bad, pattern Ok)
import Grammar.Layout (resolveLayout)
import Grammar.Par (myLexer, pProgram)
import Grammar.Print (printTree)
import Renamer.Renamer (rename)
import ReportForall (reportForall)
import Test.Hspec (
    describe,
    hspec,
    shouldBe,
    shouldNotSatisfy,
    shouldSatisfy,
    shouldThrow,
    specify,
 )
import TypeChecker.ReportTEVar (reportTEVar)
import TypeChecker.TypeChecker (TypeChecker (Bi, Hm))
import TypeChecker.TypeCheckerBidir (typecheck)
import TypeChecker.TypeCheckerIr qualified as T

test = hspec testAnnForall

testAnnForall = describe "Test AnnForall" $ do
    ann_data1
    ann_data2
    ann_bad_data1
    ann_bad_data2
    ann_bad_data3
    ann_sig1
    ann_sig2
    ann_bind

ann_data1 =
    specify "Annotate data type" $
        D.do
            "data Either a b where"
            "    Left  : a -> Either a b"
            "    Right : b -> Either a b"
            `shouldBePrg` D.do
                "data forall a. forall b. Either a b where"
                "    Left  : a -> Either a b"
                "    Right : b -> Either a b"

ann_data2 =
    specify "Annotate constructor with additional type variable" $
        D.do
            "data forall a. forall b. Either a b where"
            "    Left  : c -> a -> Either a b"
            "    Right : b -> Either a b"
            `shouldBePrg` D.do
                "data forall a. forall b. Either a b where"
                "    Left  : forall c. c -> a -> Either a b"
                "    Right : b -> Either a b"

ann_bad_data1 =
    specify "Bad data type variables" $
        D.do
            "data Either Int b where"
            "    Left  : a -> Either a b"
            "    Right : b -> Either a b"
            `shouldBeErr` "Misformed data declaration: Non type variable argument"

ann_bad_data2 =
    specify "Bad data identifer" $
        D.do
            "data Int -> Either a b where"
            "    Left  : a -> Either a b"
            "    Right : b -> Either a b"
            `shouldBeErr` "Misformed data declaration"

ann_bad_data3 =
    specify "Constructor forall duplicate" $
        D.do
            "data Int -> Either a b where"
            "    Left  : forall a. a -> Either a b"
            "    Right : b -> Either a b"
            `shouldBeErr` "Misformed data declaration"

ann_sig1 =
    specify "Annotate signature" $
        "f : a -> b -> (forall a. a -> a) -> a"
            `shouldBePrg` "f : forall a. forall b. a -> b -> (forall a. a -> a) -> a"

ann_sig2 =
    specify "Annotate signature 2" $
        D.do
            "const : forall a. forall b. a -> b -> a"
            "const x y = x"
            "main = const 'a' 65"
            `shouldBePrg` D.do
                "const : forall a. forall b. a -> b -> a"
                "const x y = x"
                "main = const 'a' 65"

ann_bind =
    specify "Annotate bind" $
        "f = (\\x.\\y. x : a -> b -> a) 4"
            `shouldBePrg` "f = (\\x.\\y. x : forall a. forall b. a -> b -> a) 4"

shouldBeErr s err = run s `shouldBe` Bad err

shouldBePrg s1 s2
    | Ok p2 <- run' s2 = run s1 `shouldBe` Ok p2
    | otherwise = error ("Faulty expectation \n" ++ show (run' s2))

run = annotateForall <=< run'
run' s = do
    p <- run'' s
    reportForall Bi p
    pure p
run'' = pProgram . resolveLayout True . myLexer

runPrint = (putStrLn . either show printTree . run) $
    D.do
        "data forall a. forall b. Either a b where"
        "    Left  : c -> a -> Either a b"
        "    Right : b -> Either a b"
