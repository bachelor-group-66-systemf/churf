{-# LANGUAGE LambdaCase #-}

module Renamer.RenamerIr (
    RExp (..),
    RBind (..),
    RProgram (..),
    Ident (..),
    Type (..),
) where

import Grammar.Abs (
    Bind (..),
    Ident (..),
    Program (..),
    Type (..),
 )
import Grammar.Print

data RProgram = RProgram [RBind]
    deriving (Eq, Show, Read, Ord)

data RBind = RBind Ident RExp
    deriving (Eq, Show, Read, Ord)

data RExp
    = RAnn RExp Type
    | RId Ident
    | RInt Integer
    | RApp RExp RExp
    | RAdd RExp RExp
    | RAbs Integer Ident RExp
    deriving (Eq, Ord, Show, Read)
