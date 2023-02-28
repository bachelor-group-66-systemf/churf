{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

import           Data.Either               (isLeft, isRight)
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Grammar.Abs
import           Test.Hspec
import           Test.QuickCheck
import           TypeChecker.TypeChecker
import qualified TypeChecker.TypeCheckerIr as T
import           TypeChecker.TypeCheckerIr (Ctx (..), Env (..), Error, Infer,
                                            Poly (..))

main :: IO ()
main = hspec $ do
    infer_elit
    infer_eann
    infer_eid
    infer_eabs
    infer_eapp

infer_elit = describe "algoW used on ELit" $ do
    it "infers the type mono Int" $ do
        getType (ELit (LInt 0)) `shouldBe` Right (TMono "Int")

    it "infers the type mono Int" $ do
        getType (ELit (LInt 9999)) `shouldBe` Right (TMono "Int")

infer_eann = describe "algoW used on EAnn" $ do
    it "infers the type and checks if the annotated type matches" $ do
        getType (EAnn (ELit $ LInt 0) (TMono "Int")) `shouldBe` Right (TMono "Int")

    it "fails if the annotated type does not match with the inferred type" $ do
        getType (EAnn (ELit $ LInt 0) (TPol "a")) `shouldSatisfy` isLeft

    it "should be possible to annotate with a more specific type" $ do
        let annotated_lambda = EAnn (EAbs "x" (EId "x")) (TArr (TMono "Int") (TMono "Int"))
         in getType annotated_lambda `shouldBe` Right (TArr (TMono "Int") (TMono "Int"))

    it "should fail if the annotated type is more general than the inferred type" $ do
        getType (EAnn (ELit (LInt 0)) (TPol "a")) `shouldSatisfy` isLeft

    it "should fail if the annotated type is an arrow but the annotated type is not" $ do
        getType (EAnn (EAbs "x" (EId "x")) (TPol "a")) `shouldSatisfy` isLeft

infer_eid = describe "algoW used on EId" $ do
    it "should fail if the variable is not added to the environment" $ do
        property $ \x -> getType (EId (Ident (x :: String))) `shouldSatisfy` isLeft

    it "should succeed if the type exist in the environment" $ do
        property $ \x -> do
            let env = Env 0 mempty mempty
            let t = Forall [] (TPol "a")
            let ctx = Ctx (M.singleton (Ident (x :: String)) t)
            getTypeC env ctx (EId (Ident x)) `shouldBe` Right (TPol "a")

infer_eabs = describe "algoW used on EAbs" $ do
    it "should infer the argument type as int if the variable is used as an int" $ do
        let lambda = EAbs "x" (EAdd (EId "x") (ELit (LInt 0)))
        getType lambda `shouldBe` Right (TArr (TMono "Int") (TMono "Int"))

    it "should infer the argument type as polymorphic if it is not used in the lambda" $ do
        let lambda = EAbs "x" (ELit (LInt 0))
        getType lambda `shouldSatisfy` isArrowPolyToMono
    it "should infer a variable as function if used as one" $ do
        let lambda = EAbs "f" (EAbs "x" (EApp (EId "f") (EId "x")))
        let isOk (Right (TArr (TArr (TPol _) (TPol _)) (TArr (TPol _) (TPol _)))) = True
            isOk _                                                                = False
        getType lambda `shouldSatisfy` isOk

infer_eapp = describe "algoW used on EApp" $ do
    it "should fail if a variable is applied to itself (occurs check)" $ do
        property $ \x -> do
            let env = Env 0 mempty mempty
            let t = Forall [] (TPol "a")
            let ctx = Ctx (M.singleton (Ident (x :: String)) t)
            getTypeC env ctx (EApp (EId (Ident x)) (EId (Ident x))) `shouldBe` Left "Occurs check failed"

isArrowPolyToMono :: Either Error Type -> Bool
isArrowPolyToMono (Right (TArr (TPol _) (TMono _))) = True
isArrowPolyToMono _                                 = False

-- | Empty environment
getType :: Exp -> Either Error Type
getType e = pure fst <*> run (inferExp e)

-- | Custom environment
getTypeC :: Env -> Ctx -> Exp -> Either Error Type
getTypeC env ctx e = pure fst <*> runC env ctx (inferExp e)