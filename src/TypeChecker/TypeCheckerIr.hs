{-# LANGUAGE LambdaCase #-}

module TypeChecker.TypeCheckerIr
    ( Program(..)
    , Bind(..)
    , Ident
    , Type(..)
    , Const(..)
    , Exp(..)
    )
        where

import Grammar.Abs (Program(..), Bind(..), Ident, Type(..), Const(..))

data Exp
    = EAnn Exp Type
    | EId Ident Type
    | EConst Const Type
    | EApp Exp Exp Type
    | EAdd Exp Exp Type
    | EAbs Ident Exp Type
  deriving (Eq, Ord, Show, Read)
