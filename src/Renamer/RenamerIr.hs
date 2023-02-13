{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Renamer.RenamerIr where

import           Grammar.Abs   (Ident, Type (..))
import           Grammar.Print


data Program = Program [Def] Main
  deriving (Eq, Ord, Show, Read)

newtype Main = Main Exp
  deriving (Eq, Ord, Show, Read)


newtype Def = DBind Bind
  deriving (Eq, Ord, Show, Read)

data Name = Nu Unique | Ni Ident deriving (Ord, Show, Eq, Read)

newtype Unique = Unique Int deriving (Enum, Eq, Read, Ord)
instance Show Unique where show (Unique i) = "x" ++ show i

data Exp
    = EId  Name
    | EInt Integer
    | EApp Exp Exp
    | EAdd Exp Exp
    | EAbs Unique Type Exp
  deriving (Eq, Ord, Show, Read)


data Bind = Bind Ident Type Exp
  deriving (Eq, Ord, Show, Read)


instance Print Program where
  prt i = \case
    Program defs main -> prPrec i 0 (concatD [prt 0 defs, prt 0 main])


instance Print Def where
  prt i (DBind b) = prPrec i 0 $ concatD [prt 0 b, doc (showString ";")]

instance Print Bind where
  prt i = \case
    Bind x t e -> prPrec i 0 $ concatD
      [ prt 0 x
      , doc (showString ":")
      , prt 0 t
      , doc (showString "=")
      , prt 0 e]

instance Print [Def] where
  prt _ []     = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]


instance Print Main where
  prt i = \case
    Main exp -> prPrec i 0 $ concatD
     [ doc (showString "main")
     , doc (showString "=")
     , prt 0 exp
     , doc (showString ";")
     ]

instance Print Exp where
  prt i = \case
    EId u -> prPrec i 3 (concatD [prt 0 u])
    EInt n -> prPrec i 3 (concatD [prt 0 n])
    EApp e e1 -> prPrec i 2 (concatD [prt 2 e, prt 3 e1])
    EAdd e e1 -> prPrec i 1 (concatD [prt 1 e, doc (showString "+"), prt 2 e1])
    EAbs u t e -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 u, doc (showString ":"), prt 0 t, doc (showString "."), prt 0 e])


instance Print Name where
  prt _ = \case
    Ni i -> prt 0 i
    Nu u -> prt 0 u

instance Print Unique where
  prt _ = doc . showString . show
