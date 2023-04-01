module TypeChecker.TypeChecker (typecheck, TypeChecker (..)) where

import Control.Monad ((<=<))
import Grammar.Abs
import Grammar.ErrM (Err)
import TypeChecker.RemoveTEVar (RemoveTEVar (rmTEVar))
import TypeChecker.TypeCheckerBidir qualified as Bi
import TypeChecker.TypeCheckerHm qualified as Hm
import TypeChecker.TypeCheckerIr qualified as T

data TypeChecker = Bi | Hm

typecheck :: TypeChecker -> Program -> Err T.Program
typecheck tc = rmTEVar <=< f
  where
    f = case tc of
        Bi -> Bi.typecheck
        Hm -> fmap fst . Hm.typecheck
