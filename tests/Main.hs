module Main where

import           Test.Hspec
import           TestAnnForall        (testAnnForall)
import           TestRenamer          (testRenamer)
import           TestReportForall     (testReportForall)
import           TestTypeCheckerBidir (testTypeCheckerBidir)
import           TestTypeCheckerHm    (testTypeCheckerHm)

main = hspec $ do
  testReportForall
  testAnnForall
  testRenamer
  testTypeCheckerBidir
  testTypeCheckerHm

