{-# OPTIONS_GHC -Wno-unused-imports #-}
module TypeChecker.CheckInj where

import           TypeChecker.TypeChecker
import qualified TypeChecker.TypeCheckerIr as T
import           TypeChecker.TypeCheckerIr (Infer)

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity     (Identity, runIdentity)
import           Data.Map                  (Map)
import qualified Data.Map                  as M

import           Grammar.Abs
import           Grammar.Print             (printTree)


checkInj :: Inj -> Infer T.Inj
checkInj (Inj it expr) = do
    (_, e') <- inferExp expr
    t' <- initType it
    return $ T.Inj (it, t') e'

initType :: Init -> Infer Type
initType = undefined

