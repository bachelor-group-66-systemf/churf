{-# LANGUAGE LambdaCase #-}

module TypeChecker.TypeCheckerIr (module Grammar.Abs, Exp) where

import Grammar.Abs (Program(..), Ident(..), Bind(..), Const(..), Type(..), UIdent(..), LIdent(..))

data Exp
    = EAnn Exp Type
    | EId Ident Type
    | EConst Const Type
    | EApp Exp Exp Type
    | EAdd Exp Exp Type
    | EAbs Ident Exp Type
  deriving (Eq, Ord, Show, Read)
