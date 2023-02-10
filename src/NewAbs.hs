{-# LANGUAGE GADTs, LambdaCase #-}

module NewAbs where

import Grammar.Abs ( Ident(..), Type )

data CExp where
    CId  :: Type -> Ident -> CExp
    CInt :: Type -> Int -> CExp
    CAdd :: Type -> CExp -> CExp -> CExp
    CAbs :: Type -> Ident -> Type -> CExp -> CExp
    CApp :: Type -> CExp -> CExp -> CExp 

instance Show CExp where
    show :: CExp -> String
    show = \case
        CId _ (Ident a) -> show a
        CInt _ i -> show i
        CAdd _ e1 e2 -> show e1 <> " + " <> show e2
        CAbs t1 i t2 e -> appendType t1 $ show "\\" <> show i <> " : " <> show t2 <> ". " <> show e
        CApp _ e1 e2 -> show e1 <> " " <> show e2

appendType :: Type -> String -> String
appendType t s = s <> " : " <> show t

data CDef = CDef Ident Type Ident [Ident] CExp
    deriving Show

newtype CProgram = CProgram [CDef]
