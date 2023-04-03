module TypeChecker.TypeChecker (typecheck, TypeChecker (..)) where

import           Control.Monad                ((<=<))
import qualified Grammar.Abs                  as G
import           Grammar.ErrM                 (Err)
import           TypeChecker.RemoveForall     (removeForall)
import qualified TypeChecker.ReportTEVar      as R
import           TypeChecker.ReportTEVar      (reportTEVar)
import qualified TypeChecker.TypeCheckerBidir as Bi
import qualified TypeChecker.TypeCheckerHm    as Hm
import           TypeChecker.TypeCheckerIr

data TypeChecker = Bi | Hm deriving Eq

typecheck :: TypeChecker -> G.Program -> Err Program
typecheck tc = fmap removeForall . (reportTEVar <=< f)
  where
    f = case tc of
        Bi -> Bi.typecheck
        Hm -> fmap fst . Hm.typecheck
