{-# LANGUAGE LambdaCase #-}

module TypeChecker.TypeCheckerIr where

import Renamer.RenamerIr

data TProgram = TProgram [TBind]

data TBind = TBind Ident Type TExp

data TExp
    = TAnn TExp Type
    | TBound Integer Ident Type
    | TFree Ident Type
    | TConst Const Type
    | TApp TExp TExp Type
    | TAdd TExp TExp Type
    | TAbs Integer Ident TExp Type
    deriving (Eq, Ord, Show, Read)
