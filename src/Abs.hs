{-# LANGUAGE TypeFamilies, PatternSynonyms, StandaloneDeriving #-}

module Abs where

import Data.String

data Program a = Program [Bind a]

data Bind a = Bind Ident [Ident] (Exp a)

newtype Ident = Ident String
    deriving (Eq, Ord, Show, Data.String.IsString)

data Exp a = EId  (IdFamily  a)  Ident
           | EInt (IntFamily a) Integer
           | EAdd (AddFamily a) (Exp a) (Exp a)
           | EApp (AppFamily a) (Exp a) (Exp a)
           | EAbs (AbsFamily a) Ident (Exp a)
           | ELet (LetFamily a) [Bind a] (Exp a)
           | EExp (ExpFamily a) (Exp a)

type family IdFamily  a
type family IntFamily a
type family AddFamily a
type family AppFamily a
type family AbsFamily a
type family LetFamily a
type family ExpFamily a
