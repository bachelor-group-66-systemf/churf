{-# LANGUAGE LambdaCase #-}

module TypeChecker.TypeCheckerIr (
    TProgram (..),
    TBind (..),
    TExp (..),
    RProgram (..),
    RBind (..),
    RExp (..),
    Type (..),
    Const (..),
    Ident (..),
) where

import           Grammar.Print
import           Renamer.RenamerIr

newtype TProgram = TProgram [TBind]
    deriving (Eq, Show, Read, Ord)

data TBind = TBind Ident Type TExp
    deriving (Eq, Show, Read, Ord)

data TExp
    = TAnn TExp Type
    | TBound Integer Ident Type
    | TFree Ident Type
    | TConst Const Type
    | TApp TExp TExp Type
    | TAdd TExp TExp Type
    | TAbs Integer Ident TExp Type
    deriving (Eq, Ord, Show, Read)

instance Print TProgram where
    prt i = \case
        TProgram defs -> prPrec i 0 (concatD [prt 0 defs])

instance Print TBind where
    prt i = \case
        TBind x t e ->
            prPrec i 0 $
                concatD
                    [ prt 0 x
                    , doc (showString ":")
                    , prt 0 t
                    , doc (showString "=")
                    , prt 0 e
                    , doc (showString "\n")
                    ]

instance Print TExp where
    prt i = \case
        TAnn e t ->
            prPrec i 2 $
                concatD
                    [ prt 0 e
                    , doc (showString ":")
                    , prt 1 t
                    ]
        TBound _ u t -> prPrec i 3 $ concatD [prt 0 u]
        TFree u t -> prPrec i 3 $ concatD [prt 0 u]
        TConst c _ -> prPrec i 3 (concatD [prt 0 c])
        TApp e e1 t -> prPrec i 2 $ concatD [prt 2 e, prt 3 e1]
        TAdd e e1 t -> prPrec i 1 $ concatD [prt 1 e, doc (showString "+"), prt 2 e1]
        TAbs _ u e t ->
            prPrec i 0 $
                concatD
                    [ doc (showString "(")
                    , doc (showString "λ")
                    , prt 0 u
                    , doc (showString ".")
                    , prt 0 e
                    , doc (showString ")")
                    ]
