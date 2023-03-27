{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestTypeCheckerHm (testTypeCheckerHm) where

import Control.Monad ((<=<))
import DoStrings qualified as D
import Grammar.Par (myLexer, pProgram)
import Test.Hspec
import Prelude (
    Bool (..),
    Either (..),
    IO,
    fmap,
    not,
    ($),
    (.),
 )

-- import Test.QuickCheck
import TypeChecker.TypeCheckerHm (typecheck)

testTypeCheckerHm = describe "Hillner Milner type checker test" $ do
    ok1
    ok2
    bad1
    bad2

-- bad3

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
    specify "Using a concrete function on a skolem variable should not succeed" $
        run
            ( D.do
                bool
                _not
                "f : a -> Bool () ;"
                " f x = not x ;"
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
