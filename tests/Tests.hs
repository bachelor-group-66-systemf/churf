{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

import Data.Either (isLeft, isRight)
import Data.Map (Map)
import Data.Map qualified as M
import Grammar.Abs
import Test.Hspec
import Test.QuickCheck
import TypeChecker.TypeChecker
import TypeChecker.TypeCheckerIr (
    Ctx (..),
    Env (..),
    Error,
    Infer,
 )
import TypeChecker.TypeCheckerIr qualified as T

main :: IO ()
main = hspec $ do
    infer_elit
    infer_eann
    infer_eid
    infer_eabs
    test_id_function

infer_elit = describe "algoW used on ELit" $ do
    it "infers the type mono Int" $ do
        getType (ELit (LInt 0)) `shouldBe` Right (T.TLit "Int")

    it "infers the type mono Int" $ do
        getType (ELit (LInt 9999)) `shouldBe` Right (T.TLit "Int")

infer_eann = describe "algoW used on EAnn" $ do
    it "infers the type and checks if the annotated type matches" $ do
        getType (EAnn (ELit $ LInt 0) (TLit "Int")) `shouldBe` Right (T.TLit "Int")

    it "fails if the annotated type does not match with the inferred type" $ do
        getType (EAnn (ELit $ LInt 0) (TVar $ MkTVar "a")) `shouldSatisfy` isLeft

    it "should be possible to annotate with a more specific type" $ do
        let annotated_lambda = EAnn (EAbs "x" (EVar "x")) (TFun (TLit "Int") (TLit "Int"))
         in getType annotated_lambda `shouldBe` Right (T.TFun (T.TLit "Int") (T.TLit "Int"))

    it "should fail if the annotated type is more general than the inferred type" $ do
        getType (EAnn (ELit (LInt 0)) (TVar $ MkTVar "a")) `shouldSatisfy` isLeft

    it "should fail if the annotated type is an arrow but the annotated type is not" $ do
        getType (EAnn (EAbs "x" (EVar "x")) (TVar $ MkTVar "a")) `shouldSatisfy` isLeft

infer_eid = describe "algoW used on EVar" $ do
    it "should fail if the variable is not added to the environment" $ do
        property $ \x -> getType (EVar (LIdent (x :: String))) `shouldSatisfy` isLeft

    it "should succeed if the type exist in the environment" $ do
        property $ \x -> do
            let env = Env 0 mempty mempty
            let t = T.TVar $ T.MkTVar "a"
            let ctx = Ctx (M.singleton (T.Ident (x :: String)) t)
            getTypeC env ctx (EVar (LIdent x)) `shouldBe` Right (T.TVar $ T.MkTVar "a")

infer_eabs = describe "algoW used on EAbs" $ do
    it "should infer the argument type as int if the variable is used as an int" $ do
        let lambda = EAbs "x" (EAdd (EVar "x") (ELit (LInt 0)))
        getType lambda `shouldBe` Right (T.TFun (T.TLit "Int") (T.TLit "Int"))

    it "should infer the argument type as polymorphic if it is not used in the lambda" $ do
        let lambda = EAbs "x" (ELit (LInt 0))
        getType lambda `shouldSatisfy` isArrowPolyToMono

    it "should infer a variable as function if used as one" $ do
        let lambda = EAbs "f" (EAbs "x" (EApp (EVar "f") (EVar "x")))
        let isOk (Right (T.TFun (T.TFun (T.TVar _) (T.TVar _)) (T.TFun (T.TVar _) (T.TVar _)))) = True
            isOk _ = False
        getType lambda `shouldSatisfy` isOk

churf_id :: Bind
churf_id = Bind "id" ["x"] (EVar "x")

churf_add :: Bind
churf_add = Bind "add" ["x", "y"] (EAdd (EVar "x") (EVar "y"))

churf_main :: Bind
churf_main = Bind "main" [] (EApp (EApp (EVar "id") (EVar "add")) (ELit (LInt 0)))

prg = Program [DBind churf_main, DBind churf_add, DBind churf_id]

test_id_function :: SpecWith ()
test_id_function =
    describe "typechecking a program with id, add and main, where id is applied to add in main" $ do
        it "should succeed to find the correct type" $ do
            typecheck prg `shouldSatisfy` isRight

isArrowPolyToMono :: Either Error T.Type -> Bool
isArrowPolyToMono (Right (T.TFun (T.TVar _) (T.TLit _))) = True
isArrowPolyToMono _ = False

-- | Empty environment
getType :: Exp -> Either Error T.Type
getType e = pure snd <*> run (inferExp e)

-- | Custom environment
getTypeC :: Env -> Ctx -> Exp -> Either Error T.Type
getTypeC env ctx e = pure snd <*> runC env ctx (inferExp e)
