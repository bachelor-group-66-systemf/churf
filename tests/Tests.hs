{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad ((<=<))
import DoStrings qualified as D
import Grammar.Par (myLexer, pProgram)
import Test.Hspec
import Prelude (Bool (..), Either (..), IO, not, ($), (.))

-- import Test.QuickCheck
import TypeChecker.TypeChecker (typecheck)

main :: IO ()
main = hspec $ do
    ok1
    ok2
    ok3
    ok4
    ok5
    bad1
    bad2
    bad3
    bad4
    bad5

ok1 =
    specify "Basic polymorphism with multiple type variables" $
        run
            ( D.do
                const
                "main = const 'a' 65 ;"
            )
            `shouldSatisfy` ok
ok2 =
    specify "Head with a correct signature is accepted" $
        run
            ( D.do
                list
                headSig
                head
            )
            `shouldSatisfy` ok

ok3 =
    specify "A basic arithmetic function should be able to be inferred" $
        run
            ( D.do
                "plusOne x = x + 1 ;"
                "main x = plusOne x ;"
            )
            `shouldBe` run
                ( D.do
                    "plusOne : Int -> Int ;"
                    "plusOne x = x + 1 ;"
                    "main : Int -> Int ;"
                    "main x = plusOne x ;"
                )

ok4 =
    specify "A basic arithmetic function should be able to be inferred" $
        run
            ( D.do
                "plusOne x = x + 1 ;"
            )
            `shouldBe` run
                ( D.do
                    "plusOne : Int -> Int ;"
                    "plusOne x = x + 1 ;"
                )

ok5 =
    specify "Most simple inference possible" $
        run
            ( D.do
                "id x = x ;"
            )
            `shouldSatisfy` ok

bad1 =
    specify "Infinite type unification should not succeed" $
        run
            ( D.do
                "main = \\x. x x ;"
            )
            `shouldSatisfy` bad

bad2 =
    specify "Pattern matching using different types should not succeed" $
        run
            ( D.do
                list
                "bad xs = case xs of {"
                "   1 => 0 ;"
                "   Nil => 0 ;"
                "};"
            )
            `shouldSatisfy` bad

bad3 =
    specify "Using a concrete function (data type) on a skolem variable should not succeed" $
        run
            ( D.do
                bool
                _not
                "f : a -> Bool () ;"
                " f x = not x ;"
            )
            `shouldSatisfy` bad

bad4 =
    specify "Using a concrete function (primitive type) on a skolem variable should not succeed" $
        run
            ( D.do
                "plusOne : Int -> Int ;"
                "plusOne x = x + 1 ;"
                "f : a -> Int ;"
                " f x = plusOne x ;"
            )
            `shouldSatisfy` bad

bad5 =
    specify "A function without signature used in an incompatible context should not succeed" $
        run
            ( D.do
                "main = id 1 2 ;"
                "id x = x ;"
            )
            `shouldSatisfy` bad

run = typecheck <=< pProgram . myLexer

ok (Right _) = True
ok (Left _) = False

bad = not . ok

-- FUNCTIONS

const = D.do
    "const : a -> b -> a ;"
    "const x y = x ;"
list = D.do
    "data List (a) where"
    "  {"
    "    Nil : List (a)"
    "    Cons : a -> List (a) -> List (a)"
    "  };"

headSig = D.do
    "head : List (a) -> a ;"

head = D.do
    "head xs = "
    "  case xs of {"
    "    Cons x xs => x ;"
    "  };"

bool = D.do
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

{-
    [a, b, c] | (Int -> Int)
    (a -> (b -> (c -> (Int -> Int))))
-}
