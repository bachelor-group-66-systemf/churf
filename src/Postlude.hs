{-# LANGUAGE OverloadedStrings #-}

module Postlude (postlude) where

import Grammar.Abs
import Prelude hiding (mod)

postlude :: Program -> Program
postlude (Program defs) = Program $ add : sub : mod : defs

add = DBind $ Bind (VarNameSymbol $ Symbol "$add") ["x", "y"] (EApp (EApp (EVar $ VarNameSymbol $ Symbol "$add") (EVar $ VarNameLIdent $ LIdent "x")) (EVar $ VarNameLIdent $ LIdent "y"))

-- add = DBind $ Bind (VarNameSymbol $ Symbol "+") ["x", "y"] (EAdd (EVar $ VarNameLIdent $ LIdent "x") (EVar $ VarNameLIdent $ LIdent "y"))
sub = DBind $ Bind (VarNameSymbol $ Symbol "$minus") ["x", "y"] (EApp (EApp (EVar $ VarNameSymbol $ Symbol "$minus") (EVar $ VarNameLIdent $ LIdent "x")) (EVar $ VarNameLIdent $ LIdent "y"))
mod = DBind $ Bind (VarNameSymbol $ Symbol "$mod") ["x", "y"] (EApp (EApp (EVar $ VarNameSymbol $ Symbol "$mod") (EVar $ VarNameLIdent $ LIdent "x")) (EVar $ VarNameLIdent $ LIdent "y"))
