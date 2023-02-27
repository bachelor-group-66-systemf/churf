{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import           Control.Monad.Except
import           Grammar.Abs
import           Test.QuickCheck
import           TypeChecker.TypeChecker
import qualified TypeChecker.TypeCheckerIr as T

main :: IO ()
main = do
    quickCheck prop_isInt
    quickCheck prop_idAbs_generic

newtype AbsExp = AE Exp deriving Show
newtype EIntExp = EI Exp deriving Show

instance Arbitrary EIntExp where
  arbitrary = genInt

instance Arbitrary AbsExp where
  arbitrary = genLambda

getType :: Infer (Type, T.Exp) -> Either Error Type
getType ie = case run ie of
    Left err    -> Left err
    Right (t,e) -> return t

genInt :: Gen EIntExp
genInt = EI . ELit . LInt <$> arbitrary

genLambda :: Gen AbsExp
genLambda = do
    str <- arbitrary @String
    let str' = Ident str
    return $ AE $ EAbs str' (EId str')

prop_idAbs_generic :: AbsExp -> Bool
prop_idAbs_generic (AE e) = case getType (inferExp e) of
    Left _  -> False
    Right t -> isGenericArr t

prop_isInt :: EIntExp -> Bool
prop_isInt (EI e) = case getType (inferExp e) of
    Left _  -> False
    Right t -> t == int

int :: Type
int = TMono "Int"

isGenericArr :: Type -> Bool
isGenericArr (TArr (TPol a) (TPol b)) = a == b
isGenericArr _                        = False
