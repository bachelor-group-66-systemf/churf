{-# LANGUAGE PatternSynonyms #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TestReportForall (testReportForall, test) where

import           AnnForall               (annotateForall)
import           Control.Monad           ((<=<))
import qualified DoStrings               as D
import           Grammar.ErrM            (Err, pattern Bad, pattern Ok)
import           Grammar.Layout          (resolveLayout)
import           Grammar.Par             (myLexer, pProgram)
import           Grammar.Print           (printTree)
import           Renamer.Renamer         (rename)
import           ReportForall            (reportForall)
import           Test.Hspec              (describe, hspec, shouldBe,
                                          shouldNotSatisfy, shouldSatisfy,
                                          shouldThrow, specify)
import           TypeChecker.TypeChecker (TypeChecker (Bi, Hm))

testReportForall = describe "Test ReportForall" $ do
    rp_unused1
    rp_unused2
    rp_forall

test = hspec testReportForall

rp_unused1 = specify "Unused forall 1" $
    "g : forall a. forall a. a -> (forall a. a -> a) -> a"
    `shouldBeErrBi`
    "Unused forall"

rp_unused2 = specify "Unused forall 2" $
    "g : forall a. (forall a. a -> a) -> Int"
    `shouldBeErrBi`
    "Unused forall"

rp_forall = specify "Rank2 forall with Hm" $
    "f : a -> b -> (forall a. a -> a) -> a"
    `shouldBeErrHm`
    "Higher rank forall not allowed"

shouldBeErrBi = shouldBeErr Bi
shouldBeErrHm = shouldBeErr Hm
shouldBeErr tc s err = run tc s `shouldBe` Bad err

run tc = reportForall tc <=< pProgram . resolveLayout True . myLexer
