{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TestTypeCheckerBidir (test, testTypeCheckerBidir) where

import           Test.Hspec

import           Control.Monad                ((<=<))
import           Grammar.ErrM                 (Err, pattern Bad, pattern Ok)
import           Grammar.Par                  (myLexer, pProgram)
import           Renamer.Renamer              (rename)
import           TypeChecker.RemoveTEVar      (RemoveTEVar (rmTEVar))
import           TypeChecker.TypeCheckerBidir (typecheck)
import qualified TypeChecker.TypeCheckerIr    as T


test = hspec testTypeCheckerBidir

testTypeCheckerBidir = describe "Bidirectional type checker test" $ do
    tc_id
    tc_double
    tc_add_lam
    tc_const
    tc_simple_rank2
    tc_rank2
    tc_identity
    tc_pair
    tc_tree
    tc_mono_case
    tc_pol_case
    tc_mut_rec
    tc_infer_case

tc_id =
    specify "Basic identity function polymorphism" $
        run
            [ "id : forall a. a -> a;"
            , "id x = x;"
            , "main = id 4;"
            ]
            `shouldSatisfy` ok

tc_double =
    specify "Addition inference" $
        run
            ["double x = x + x;"]
            `shouldSatisfy` ok

tc_add_lam =
    specify "Addition lambda inference" $
        run
            ["four = (\\x. x + x) 2;"]
            `shouldSatisfy` ok

tc_const =
    specify "Basic polymorphism with multiple type variables" $
        run
            [ "const : forall a. forall b. a -> b -> a;"
            , "const x y = x;"
            , "main = const 'a' 65;"
            ]
            `shouldSatisfy` ok

tc_simple_rank2 =
    specify "Simple rank two polymorphism" $
        run
            [ "id : forall a. a -> a;"
            , "id x = x;"
            , "f : forall a. a -> (forall b. b -> b) -> a;"
            , "f x g = g x;"
            , "main = f 4 id;"
            ]
            `shouldSatisfy` ok

tc_rank2 =
    specify "Rank two polymorphism is ok" $
        run
            [ "const : forall a. forall b. a -> b -> a;"
            , "const x y = x;"
            , "rank2 : forall a. forall b. a -> (forall c. c -> Int) -> b -> Int;"
            , "rank2 x f y = f x + f y;"
            , "main = rank2 3 (\\x. const 5 x : forall a. a -> Int) 'h';"
            ]
            `shouldSatisfy` ok

tc_identity = describe "(∀b. b → b) should only accept the identity function" $ do
    specify "identityᵢₙₜ is rejected" $ run (fs ++ id_int) `shouldNotSatisfy` ok
    specify "identity is accepted" $ run (fs ++ id) `shouldSatisfy` ok
  where
    fs =
        [ "f : forall a. a -> (forall b. b -> b) -> a;"
        , "f x g = g x;"
        , "id : forall a. a -> a;"
        , "id x = x;"
        , "id_int : Int -> Int;"
        , "id_int x = x;"
        ]
    id =
        [ "main : Int;"
        , "main = f 4 id;"
        ]
    id_int =
        [ "main : Int;"
        , "main = f 4 id_int;"
        ]

tc_pair = describe "Pair. Type variables in Pair a b typechecked" $ do
    specify "Wrong arguments are rejected" $ run (fs ++ wrong) `shouldNotSatisfy` ok
    specify "Correct arguments are accepted" $ run (fs ++ correct) `shouldSatisfy` ok
  where
    fs =
        [ "data forall a. forall b. Pair (a  b) where {"
        , "  Pair : a -> b -> Pair (a b)"
        , "};"
        , "main : Pair (Int Char);"
        ]
    wrong = ["main = Pair 'a' 65;"]
    correct = ["main = Pair 65 'a';"]

tc_tree = describe "Tree. Recursive data type" $ do
    specify "Wrong tree is rejected" $ run (fs ++ wrong) `shouldNotSatisfy` ok
    specify "Correct tree is accepted" $ run (fs ++ correct) `shouldSatisfy` ok
  where
    fs =
        [ "data forall a. Tree (a) where {"
        , "  Node : a -> Tree (a) -> Tree (a) -> Tree (a)"
        , "  Leaf : a -> Tree (a)"
        , "};"
        ]
    wrong = ["tree = Node 1 (Node 2 (Node 4) (Leaf 5)) (Leaf 3);"]
    correct = ["tree = Node 1 (Node 2 (Leaf 4) (Leaf 5)) (Leaf 3);"]

tc_mono_case = describe "Monomorphic pattern matching" $ do
    specify "First wrong case expression rejected" $
        run wrong1 `shouldNotSatisfy` ok
    specify "Second wrong case expression rejected" $
        run wrong2 `shouldNotSatisfy` ok
    specify "Third wrong case expression rejected" $
        run wrong3 `shouldNotSatisfy` ok
    specify "First correct case expression accepted" $
        run correct1 `shouldSatisfy` ok
    specify "Second correct case expression accepted" $
        run correct2 `shouldSatisfy` ok
  where
    wrong1 =
        [ "simple : Int -> Int;"
        , "simple c = case c of {"
        , "  'F' => 0;"
        , "  'T' => 1;"
        , "};"
        ]
    wrong2 =
        [ "simple : Char -> Int;"
        , "simple c = case c of {"
        , "  'F' => 0;"
        , "   1  => 1;"
        , "};"
        ]
    wrong3 =
        [ "simple : Char -> Int;"
        , "simple c = case c of {"
        , "  'F' => 0;"
        , "  'T' => '1';"
        , "};"
        ]
    correct1 =
        [ "simple : Char -> Int;"
        , "simple c = case c of {"
        , "  'F' => 0;"
        , "  'T' => 1;"
        , "};"
        ]
    correct2 =
        [ "simple : Char -> Int;"
        , "simple c = case c of {"
        , "  'F' => 0;"
        , "   _  => 1;"
        , "};"
        ]

tc_pol_case = describe "Polymophic and recursive pattern matching" $ do
    specify "First wrong case expression rejected" $
        run (fs ++ wrong1) `shouldNotSatisfy` ok
    specify "Second wrong case expression rejected" $
        run (fs ++ wrong2) `shouldNotSatisfy` ok
    specify "Third wrong case expression rejected" $
        run (fs ++ wrong3) `shouldNotSatisfy` ok
    specify "Forth wrong case expression rejected" $
        run (fs ++ wrong4) `shouldNotSatisfy` ok
    specify "First correct case expression accepted" $
        run (fs ++ correct1) `shouldSatisfy` ok
    specify "Second correct case expression accepted" $
        run (fs ++ correct2) `shouldSatisfy` ok
    specify "Third correct case expression accepted" $
        run (fs ++ correct3) `shouldSatisfy` ok
    specify "Forth correct case expression accepted" $
        run (fs ++ correct4) `shouldSatisfy` ok
  where
    fs =
        [ "data forall a. List (a) where {"
        , "  Nil  : List (a)"
        , "  Cons : a -> List (a) -> List (a)"
        , "};"
        ]
    wrong1 =
        [ "length : forall c. List (c) -> Int;"
        , "length = \\list. case list of {"
        , "  Nil       => 0;"
        , "  Cons 6 xs => 1 + length xs;"
        , "};"
        ]
    wrong2 =
        [ "length : forall c. List (c) -> Int;"
        , "length = \\list. case list of {"
        , "  Cons      => 0;"
        , "  Cons x xs => 1 + length xs;"
        , "};"
        ]
    wrong3 =
        [ "length : forall c. List (c) -> Int;"
        , "length = \\list. case list of {"
        , "  0         => 0;"
        , "  Cons x xs => 1 + length xs;"
        , "};"
        ]
    wrong4 =
        [ "elems : forall c. List (List(c)) -> Int;"
        , "elems = \\list. case list of {"
        , "  Nil                => 0;"
        , "  Cons Nil Nil       => 0;"
        , "  Cons Nil xs        => elems xs;"
        , "  Cons (Cons Nil ys) xs => 1 + elems (Cons ys xs);"
        , "};"
        ]
    correct1 =
        [ "length : forall c. List (c) -> Int;"
        , "length = \\list. case list of {"
        , "  Nil                 => 0;"
        , "  Cons x xs           => 1 + length xs;"
        , "  Cons x (Cons y Nil) => 2;"
        , "};"
        ]
    correct2 =
        [ "length : forall c. List (c) -> Int;"
        , "length = \\list. case list of {"
        , "  Nil       => 0;"
        , "  non_empty => 1;"
        , "};"
        ]
    correct3 =
        [ "length : List (Int) -> Int;"
        , "length = \\list. case list of {"
        , "  Nil                => 0;"
        , "  Cons 1 Nil         => 1;"
        , "  Cons x (Cons 2 xs) => 2 + length xs;"
        , "};"
        ]
    correct4 =
        [ "elems : forall c. List (List(c)) -> Int;"
        , "elems = \\list. case list of {"
        , "  Nil                => 0;"
        , "  Cons Nil Nil       => 0;"
        , "  Cons Nil xs        => elems xs;"
        , "  Cons (Cons _ ys) xs => 1 + elems (Cons ys xs);"
        , "};"
        ]


tc_mut_rec = specify "Feasible mutuable recursive definitions" $ run
    [ "data Bool () where {"
    , "    True : Bool ()"
    , "    False : Bool ()"
    , "};"

    , "even : Int -> Bool ();"
    , "even x = not (odd x);"

    , "odd x = not (even x);"

    , "not x = case x of {"
    , "    True => False;"
    , "    False => True;"
    , "};"
    ] `shouldSatisfy` ok

tc_infer_case = describe "Infer case expression" $ do
    specify "Wrong case expression rejected" $
        run (fs ++ wrong) `shouldNotSatisfy` ok
    specify "Correct case expression accepted" $
        run (fs ++ correct) `shouldSatisfy` ok
  where
    fs =
        [ "data Bool () where {"
        , "    True : Bool ()"
        , "    False : Bool ()"
        , "};"
        ]

    correct =
        [ "toBool = case 0 of {"
        , "    0 => False;"
        , "    _ => True;"
        , "};"
        ]

    wrong =
        [ "toBool = case 0 of {"
        , "    0 => False;"
        , "    _ => 1;"
        , "};"
        ]


run :: [String] -> Err T.Program
run = rmTEVar <=< typecheck <=< pProgram . myLexer . unlines

ok = \case
    Ok _  -> True
    Bad _ -> False
