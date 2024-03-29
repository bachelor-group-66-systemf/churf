{-# LANGUAGE QualifiedDo #-}

module TestTypeCheckerHm where

import Control.Monad (sequence_, (<=<))
import Test.Hspec

import AnnForall (annotateForall)
import Desugar.Desugar (desugar)
import DoStrings qualified as D
import Grammar.Layout (resolveLayout)
import Grammar.Par (myLexer, pProgram)
import Grammar.Print (printTree)
import Renamer.Renamer (rename)
import ReportForall (reportForall)
import TypeChecker.TypeChecker (TypeChecker (Hm))
import TypeChecker.TypeCheckerHm (typecheck)
import TypeChecker.TypeCheckerIr (Program)

testTypeCheckerHm = describe "Hindley-Milner type checker test" $ do
    sequence_ goods
    sequence_ bads

goods =
    [ testSatisfy
        "Basic polymorphism with multiple type variables"
        ( D.do
            _const
            "main = const 'a' 65 ;"
        )
        ok
    , testSatisfy
        "Head with a correct signature is accepted"
        ( D.do
            _List
            _headSig
            _head
        )
        ok
    , testSatisfy
        "Most simple inference possible"
        ( D.do
            _id
        )
        ok
    , testSatisfy
        "Pattern matching on a nested list"
        ( D.do
            _List
            "main : List (List a) -> Int;"
            "main xs = case xs of {"
            "    Cons Nil _ => 1;"
            "    _ => 0;"
            "};"
        )
        ok
    , testSatisfy
        "A basic arithmetic function should be able to be inferred"
        ( D.do
            ".+ : Int -> Int -> Int"
            ".+ x y = x"
            "plusOne x = x + 1 ;"
            "main x = plusOne x ;"
        )
        ok
    , testSatisfy
        "List of function Int -> Int functions should be inferred corretly"
        ( D.do
            _List
            "main xs = case xs of {"
            "    Cons f _ => f 1 ;"
            "    Nil => 0 ;"
            " };"
        )
        ok
    , testSatisfy
        "length function on int list infers correct signature"
        ( D.do
            ".+ : Int -> Int -> Int"
            ".+ x y = x"
            "data List where "
            "    Nil : List"
            "    Cons : Int -> List -> List"

            "length xs = case xs of"
            "    Nil => 0"
            "    Cons _ xs => 1 + length xs"
        )
        ok
    ]

bads =
    [ testSatisfy
        "Infinite type unification should not succeed"
        ( D.do
            "main = \\x. x x ;"
        )
        bad
    , testSatisfy
        "Pattern matching using different types should not succeed"
        ( D.do
            _List
            "bad xs = case xs of {"
            "   1 => 0 ;"
            "   Nil => 0 ;"
            "};"
        )
        bad
    , testSatisfy
        "Using a concrete function (data type) on a skolem variable should not succeed"
        ( D.do
            _Bool
            _not
            "f : a -> Bool ;"
            "f x = not x ;"
        )
        bad
    , testSatisfy
        "Using a concrete function (primitive type) on a skolem variable should not succeed"
        ( D.do
            ".+ : Int -> Int -> Int"
            ".+ x y = x"
            "plusOne : Int -> Int ;"
            "plusOne x = x + 1 ;"
            "f : a -> Int ;"
            "f x = plusOne x ;"
        )
        bad
    , testSatisfy
        "A function without signature used in an incompatible context should not succeed"
        ( D.do
            "main = _id 1 2 ;"
            "_id x = x ;"
        )
        bad
    , testSatisfy
        "Pattern matching on literal and _List should not succeed"
        ( D.do
            _List
            ".+ : Int -> Int -> Int"
            ".+ x y = x"
            "length : List c -> Int;"
            "length _List = case _List of {"
            "  0         => 0;"
            "  Cons x xs => 1 + length xs;"
            "};"
        )
        bad
    , testSatisfy
        "List of function Int -> Int functions should not be usable on Char"
        ( D.do
            _List
            "main : List (Int -> Int) -> Int ;"
            "main xs = case xs of {"
            "    Cons f _ => f 'a' ;"
            "    Nil => 0 ;"
            " };"
        )
        bad
    , -- FIXME FAILING TEST
      testSatisfy
        "id with incorrect signature"
        ( D.do
            "id : a -> b;"
            "id x = x;"
        )
        bad
    , -- FIXME FAILING TEST
      testSatisfy
        "incorrect signature on const"
        ( D.do
            "const : a -> b -> b;"
            "const x y = x"
        )
        bad
    , -- FIXME FAILING TEST
      testSatisfy
        "incorrect type signature on id lambda"
        ( D.do
            "id = ((\\x. x) : a -> b);"
        )
        bad
    ]

testSatisfy desc test satisfaction = specify desc $ run test `shouldSatisfy` satisfaction
testBe desc test shouldbe = specify desc $ run test `shouldBe` run shouldbe

run s = do
    p <- (fmap desugar . pProgram . resolveLayout True . myLexer) s
    reportForall Hm p
    (printTree . fst) <$> (typecheck <=< rename <=< annotateForall) p

ok (Right _) = True
ok (Left _) = False

bad = not . ok

-- FUNCTIONS

_const = D.do
    "const : a -> b -> a"
    "const x y = x"
_List = D.do
    "data List a where { Nil : List a; Cons : a -> List a -> List a; }"

_headSig = D.do
    "head : List a -> a"

_head = D.do
    "head xs = case xs of"
    "    Cons x xs => x"

_Bool = D.do
    "data Bool where"
    "    True : Bool"
    "    False : Bool"

_not = D.do
    "not : Bool -> Bool ;"
    "not x = case x of"
    "    True => False"
    "    False => True"

_id = "id x = x ;"

_Maybe = D.do
    "data Maybe a where"
    "    Nothing : Maybe a"
    "    Just : a -> Maybe a"

_fmap = D.do
    "fmap f ma = case ma of"
    "    Nothing => Nothing"
    "    Just a => Just (f a)"
