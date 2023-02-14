{-# LANGUAGE LambdaCase #-}

module Renamer.RenamerIr (module Grammar.Abs, RExp (..), RBind (..), RProgram (..)) where

import Grammar.Abs (
    Bind (..),
    Const (..),
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
    | RBound Integer Ident
    | RFree Ident
    | RConst Const
    | RApp RExp RExp
    | RAdd RExp RExp
    | RAbs Integer Ident RExp
    deriving (Eq, Ord, Show, Read)

instance Print RProgram where
    prt i = \case
        RProgram defs -> prPrec i 0 (concatD [prt 0 defs])

instance Print RBind where
    prt i = \case
        RBind x e ->
            prPrec i 0 $
                concatD
                    [ prt 0 x
                    , doc (showString "=")
                    , prt 0 e
                    ]

instance Print RExp where
    prt i = \case
        RBound n _ -> prPrec i 3 (concatD [prt 0 ("var" ++ show n)])
        RFree id -> prPrec i 3 (concatD [prt 0 id])
        RConst n -> prPrec i 3 (concatD [prt 0 n])
        RApp e e1 -> prPrec i 2 (concatD [prt 2 e, prt 3 e1])
        RAdd e e1 -> prPrec i 1 (concatD [prt 1 e, doc (showString "+"), prt 2 e1])
        RAbs u id e -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 ("var" ++ show u), doc (showString "."), prt 0 e])
