{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad ((<=<))
import DoStrings qualified as D
import Grammar.Par (myLexer, pProgram)
import Test.Hspec
import Prelude (Bool (..), Either (..), IO, mapM_, not, ($), (.))

-- import Test.QuickCheck
import TypeChecker.TypeChecker (typecheck)

main :: IO ()
main = do
    mapM_ hspec goods
    mapM_ hspec bads

goods =
    [ specify "Basic polymorphism with multiple type variables" $
        run
            ( D.do
                _const
                "main = const 'a' 65 ;"
            )
            `shouldSatisfy` ok
    , specify "Head with a correct signature is accepted" $
        run
            ( D.do
                _list
                _headSig
                _head
            )
            `shouldSatisfy` ok
    , specify "A basic arithmetic function should be able to be inferred" $
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
    , specify "A basic arithmetic function should be able to be inferred" $
        run
            ( D.do
                "plusOne x = x + 1 ;"
            )
            `shouldBe` run
                ( D.do
                    "plusOne : Int -> Int ;"
                    "plusOne x = x + 1 ;"
                )
    , specify "Most simple inference possible" $
        run
            ( D.do
                _id
            )
            `shouldSatisfy` ok
    , specify "Pattern matching on a nested list" $
        run
            ( D.do
                _list
                "main : List (List (a)) -> Int ;"
                "main xs = case xs of {"
                "    Cons Nil _ => 1 ;"
                "    _ => 0 ;"
                "};"
            )
            `shouldSatisfy` ok
    , specify "List of function Int -> Int functions should be inferred corretly" $
        run
            ( D.do
                _list
                "main xs = case xs of {"
                "    Cons f _ => f 1 ;"
                "    Nil => 0 ;"
                " };"
            )
            `shouldBe` run
                ( D.do
                    _list
                    "main : List (Int -> Int) -> Int ;"
                    "main xs = case xs of {"
                    "    Cons f _ => f 1 ;"
                    "    Nil => 0 ;"
                    " };"
                )
    ]

bads =
    [ specify "Infinite type unification should not succeed" $
        run
            ( D.do
                "main = \\x. x x ;"
            )
            `shouldSatisfy` bad
    , specify "Pattern matching using different types should not succeed" $
        run
            ( D.do
                _list
                "bad xs = case xs of {"
                "   1 => 0 ;"
                "   Nil => 0 ;"
                "};"
            )
            `shouldSatisfy` bad
    , specify "Using a concrete function (data type) on a skolem variable should not succeed" $
        run
            ( D.do
                _bool
                _not
                "f : a -> Bool () ;"
                "f x = not x ;"
            )
            `shouldSatisfy` bad
    , specify "Using a concrete function (primitive type) on a skolem variable should not succeed" $
        run
            ( D.do
                "plusOne : Int -> Int ;"
                "plusOne x = x + 1 ;"
                "f : a -> Int ;"
                " f x = plusOne x ;"
            )
            `shouldSatisfy` bad
    , specify "A function without signature used in an incompatible context should not succeed" $
        run
            ( D.do
                "main = _id 1 2 ;"
                "_id x = x ;"
            )
            `shouldSatisfy` bad
    , specify "Pattern matching on literal and _list should not succeed" $
        run
            ( D.do
                _list
                "length : List (c) -> Int;"
                "length _list = case _list of {"
                "  0         => 0;"
                "  Cons x xs => 1 + length xs;"
                "};"
            )
            `shouldSatisfy` bad
    , specify "List of function Int -> Int functions should not be usable on Char" $
        run
            ( D.do
                _list
                "main : List (Int -> Int) -> Int ;"
                "main xs = case xs of {"
                "    Cons f _ => f 'a' ;"
                "    Nil => 0 ;"
                " };"
            )
            `shouldSatisfy` bad
    ]

run = typecheck <=< pProgram . myLexer

ok (Right _) = True
ok (Left _) = False

bad = not . ok

-- FUNCTIONS

_const = D.do
    "const : a -> b -> a ;"
    "const x y = x ;"
_list = D.do
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

_bool = D.do
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
