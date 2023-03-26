
module TypeChecker.TypeChecker (typecheck, TypeChecker(..)) where

import           Control.Monad                ((<=<))
import           Grammar.Abs
import           Grammar.ErrM                 (Err)
import           TypeChecker.RemoveTEVar      (RemoveTEVar (rmTEVar))
import qualified TypeChecker.TypeCheckerBidir as Bi
import qualified TypeChecker.TypeCheckerHm    as Hm
import qualified TypeChecker.TypeCheckerIr    as T


data TypeChecker = Bi | Hm

typecheck :: TypeChecker -> Program -> Err T.Program
typecheck tc = rmTEVar <=< f
  where
    f = case tc of
          Bi -> Bi.typecheck
          Hm -> Hm.typecheck
