{-# LANGUAGE LambdaCase #-}

module TypeChecker.HMIr
  ( module Grammar.Abs
  , module TypeChecker.HMIr
  ) where

import           Grammar.Abs   (Ident (..), Type (..))
import           Grammar.Print
import           Prelude
import qualified Prelude       as C (Eq, Ord, Read, Show)

newtype Program = Program [Bind]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Exp
    = EId  Type Ident
    | EInt Type Integer
    | ELet Type Ident Exp Exp
    | EApp Type Exp Exp
    | EAdd Type Exp Exp
    | EAbs Type Ident Exp
      deriving (C.Eq, C.Ord, C.Show, C.Read)

type Id = (Type, Ident)

data Bind = Bind Id [Id] Exp
    deriving (C.Eq, C.Ord, C.Show, C.Read)

instance Print Program where
    prt i (Program sc) = prPrec i 0 $ prt 0 sc

instance Print Bind where
    prt i (Bind name@(n, _) parms rhs) = prPrec i 0 $ concatD
        [ prtId 0 name
        , doc $ showString ";"
        , prt 0 n
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
    EId _ n       -> prPrec i 3 $ concatD [prt 0 n]
    EInt _ i1      -> prPrec i 3 $ concatD [prt 0 i1]
    ELet _ name e1 e2    -> prPrec i 3 $ concatD
                        [ doc $ showString "let"
                        , prt 0 name
                        , prt 0 e1
                        , doc $ showString "in"
                        , prt 0 e2
                        ]
    EApp t e1 e2 -> prPrec i 2 $ concatD
                        [ doc $ showString "@"
                        , prt 0 t
                        , prt 2 e1
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
                        , prt 0 n
                        , doc $ showString "."
                        , prt 0 e
                        ]



