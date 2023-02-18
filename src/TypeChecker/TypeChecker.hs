
module TypeChecker.TypeChecker (typecheck) where

import           Debug.Trace                  (trace)
import           Grammar.Abs
import           Grammar.ErrM                 (Err)
import           Grammar.Print                (printTree)
import           TypeChecker.RemoveTEVar      (RemoveTEVar (rmTEVar))
import qualified TypeChecker.TypeCheckerBidir as Bi
import qualified TypeChecker.TypeCheckerHm    as Hm
import qualified TypeChecker.TypeCheckerIr    as T

typecheck :: Program -> Err T.Program
typecheck p = do
  p <- Bi.typecheck p
  trace (printTree p) pure ()
  pure $ rmTEVar p

-- typecheck p = rmTEVar <$> Hm.typecheck p
