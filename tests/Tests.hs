module Main where

import           Test.Hspec
import           TestTypeCheckerBidir (testTypeCheckerBidir)
import           TestTypeCheckerHm    (testTypeCheckerHm)

main = hspec $ do
  testTypeCheckerBidir
  testTypeCheckerHm

