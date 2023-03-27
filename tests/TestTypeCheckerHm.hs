{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestTypeCheckerHm where

import Control.Monad ((<=<))
import DoStrings qualified as D
import Grammar.Par (myLexer, pProgram)
import Test.Hspec
import Prelude (Bool (..), Either (..), IO, foldl1, mapM_, not, ($), (.), (>>))

-- import Test.QuickCheck
import TypeChecker.TypeCheckerHm (typecheck)

testTypeCheckerHm = describe "Hindley-Milner type checker test" $ do
    foldl1 (>>) goods
    foldl1 (>>) bads
    foldl1 (>>) bes

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
            "main : List (List (a)) -> Int ;"
            "main xs = case xs of {"
            "    Cons Nil _ => 1 ;"
            "    _ => 0 ;"
            "};"
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
            "f : a -> Bool () ;"
            "f x = not x ;"
        )
        bad
    , testSatisfy
        "Using a concrete function (primitive type) on a skolem variable should not succeed"
        ( D.do
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
            "length : List (c) -> Int;"
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
    , testSatisfy
        "id with incorrect signature"
        ( D.do
            "id : a -> b;"
            "id x = x;"
        )
        bad
    , testSatisfy
        "incorrect type signature on id lambda"
        ( D.do
            "id = ((\\x. x) : a -> b);"
        )
        bad
    ]

bes =
    [ testBe
        "A basic arithmetic function should be able to be inferred"
        ( D.do
            "plusOne x = x + 1 ;"
            "main x = plusOne x ;"
        )
        ( D.do
            "plusOne : Int -> Int ;"
            "plusOne x = x + 1 ;"
            "main : Int -> Int ;"
            "main x = plusOne x ;"
        )
    , testBe
        "A basic arithmetic function should be able to be inferred"
        ( D.do
            "plusOne x = x + 1 ;"
        )
        ( D.do
            "plusOne : Int -> Int ;"
            "plusOne x = x + 1 ;"
        )
    , testBe
        "List of function Int -> Int functions should be inferred corretly"
        ( D.do
            _List
            "main xs = case xs of {"
            "    Cons f _ => f 1 ;"
            "    Nil => 0 ;"
            " };"
        )
        ( D.do
            _List
            "main : List (Int -> Int) -> Int ;"
            "main xs = case xs of {"
            "    Cons f _ => f 1 ;"
            "    Nil => 0 ;"
            " };"
        )
    ]

testSatisfy desc test satisfaction = specify desc $ run test `shouldSatisfy` satisfaction
testBe desc test shouldbe = specify desc $ run test `shouldBe` run shouldbe

run = typecheck <=< pProgram . myLexer

ok (Right _) = True
ok (Left _) = False

bad = not . ok

-- FUNCTIONS

_const = D.do
    "const : a -> b -> a ;"
    "const x y = x ;"
_List = D.do
    "data List (a) where"
    "  {"
    "    Nil : List (a)"
    "    Cons : a -> List (a) -> List (a)"
    "  };"

_headSig = D.do
    "head : List (a) -> a ;"

_head = D.do
    "head xs = "
    "  case xs of {"
    "    Cons x xs => x ;"
    "  };"

_Bool = D.do
    "data Bool () where {"
    "    True : Bool ()"
    "    False : Bool ()"
    "};"

_not = D.do
    "not : Bool () -> Bool () ;"
    "not x = case x of {"
    "    True => False ;"
    "    False => True ;"
    "};"
_id = "id x = x ;"

_Maybe = D.do
    "data Maybe (a) where {"
    "    Nothing : Maybe (a)"
    "    Just : a -> Maybe (a)"
    "    };"

_fmap = D.do
    "fmap f ma = case ma of {"
    "    Nothing => Nothing ;"
    "    Just a => Just (f a) ;"
    "};"
