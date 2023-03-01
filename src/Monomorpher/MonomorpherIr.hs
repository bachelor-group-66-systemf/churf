{-# LANGUAGE LambdaCase #-}

module Monomorpher.MonomorpherIr
  ( module Grammar.Abs
  , module Monomorpher.MonomorpherIr
  ) where

import           Grammar.Abs   (Ident (..), Literal (..))
import           Grammar.Print
import           Prelude
import qualified Prelude       as C (Eq, Ord, Read, Show)

newtype Program = Program [Bind]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Exp
    = EId  Id
    | ELit Type Literal
    | ELet Bind Exp
    | EApp Type Exp Exp
    | EAdd Type Exp Exp
    | EAbs Type Id Exp
      deriving (C.Eq, C.Ord, C.Read, C.Show)

type Id = (Ident, Type)

-- Custom version of type which does not include TPol
data Type = TMono Ident | TArr Type Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance Print Type where
  prt i = \case
    TMono id_ -> prPrec i 1 (concatD [doc (showString "_"), prt 0 id_])
    TArr type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])

instance Print [Type] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]



data Bind = Bind Id [Id] Exp
    deriving (C.Eq, C.Ord, C.Show, C.Read)

instance Print Program where
    prt i (Program sc) = prPrec i 0 $ prt 0 sc

instance Print Bind where
    prt i (Bind (t, name) parms rhs) = prPrec i 0 $ concatD
        [ prt 0 name
        , doc $ showString ":"
        , prt 0 t
        , prtIdPs 0 parms
        , doc $ showString "="
        , prt 0 rhs
        ]

instance Print [Bind] where
  prt _ []     = concatD []
  prt _ [x]    = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

prtIdPs :: Int -> [Id] -> Doc
prtIdPs i = prPrec i 0 . concatD . map (prtIdP i)

prtId :: Int -> Id -> Doc
prtId i (name, t) = prPrec i 0 $ concatD
    [ prt 0 name
    , doc $ showString ":"
    , prt 0 t
    ]

prtIdP :: Int -> Id -> Doc
prtIdP i (name, t) = prPrec i 0 $ concatD
    [ doc $ showString "("
    , prt 0 name
    , doc $ showString ":"
    , prt 0 t
    , doc $ showString ")"
    ]


instance Print Exp where
  prt i = \case
    EId n       -> prPrec i 3 $ concatD [prtId 0 n]
    ELit _ (LInt i1) -> prPrec i 3 $ concatD [prt 0 i1]
    ELet bs e    -> prPrec i 3 $ concatD
                    [ doc $ showString "let"
                    , prt 0 bs
                    , doc $ showString "in"
                    , prt 0 e
                    ]
    EApp t e1 e2 -> prPrec i 2 $ concatD
                        [ prt 2 e1
                        , prt 3 e2
                        ]
    EAdd t e1 e2 -> prPrec i 1 $ concatD
                        [ doc $ showString "@"
                        , prt 0 t
                        , prt 1 e1
                        , doc $ showString "+"
                        , prt 2 e2
                        ]
    EAbs t n e  -> prPrec i 0 $ concatD
                        [ doc $ showString "@"
                        , prt 0 t
                        , doc $ showString "\\"
                        , prtId 0 n
                        , doc $ showString "."
                        , prt 0 e
                        ]

