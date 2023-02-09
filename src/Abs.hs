{-# LANGUAGE TypeFamilies, StandaloneDeriving #-}

module Abs where

data Exp eps
    = EInt (XInt eps) Integer
    | EId (XId eps) String
    | EAdd (XAdd eps) (Exp eps) (Exp eps)
    | EApp (XApp eps) (Exp eps) (Exp eps)
    | EAbs (XAbs eps) String (Exp eps)
    | EExp (XExp eps)

newtype Ident = Ident String

type family XInt eps
type family XId  eps
type family XAdd eps
type family XApp eps
type family XAbs eps
type family XExp eps
