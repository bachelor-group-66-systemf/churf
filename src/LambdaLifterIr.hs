{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module LambdaLifterIr (
    module Grammar.Abs,
    module LambdaLifterIr,
    module TypeChecker.TypeCheckerIr
) where

import           Data.List                 (intercalate)
import           Grammar.Abs               (Lit (..))
import           Grammar.Print
import           Prelude                   hiding (exp)
import qualified Prelude                   as C (Eq, Ord, Show)
import           TypeChecker.TypeCheckerIr (Ident (..), TVar (..), Type (..))

newtype Program = Program [Def]
    deriving (C.Eq, C.Ord, C.Show)

data Def
    = DBind Bind
    | DData Data
    deriving (C.Eq, C.Ord, C.Show)

data Data = Data Type [Inj]
    deriving (C.Eq, C.Ord, C.Show)

data Inj = Inj Ident Type
    deriving (C.Eq, C.Ord, C.Show)

data Pattern
    = PVar Ident
    | PLit Lit
    | PCatch
    | PEnum Ident
    | PInj Ident [(Pattern, Type)]
    deriving (C.Eq, C.Ord, C.Show)

data Exp
    = EVar Ident
    | EVarC [T Ident] Ident
    | EInj Ident
    | ELit Lit
    | ELet (T Ident) (T Exp) (T Exp)
    | EApp (T Exp)(T Exp)
    | EAdd (T Exp)(T Exp)
    | ECase (T Exp) [Branch]
    deriving (C.Eq, C.Ord, C.Show)


type T a = (a, Type)

data Bind = Bind (T Ident) [T Ident] (T Exp)
          | BindC [T Ident] (T Ident) [T Ident] (T Exp)
    deriving (C.Eq, C.Ord, C.Show)

data Branch = Branch (T Pattern) (T Exp)
    deriving (C.Eq, C.Ord, C.Show)

instance Print Program where
    prt i (Program sc) = prt i sc

instance Print Bind where
    prt i (Bind sig parms rhs) = concatD
                [ prt i sig
                , prt i parms
                , doc $ showString "="
                , prt i rhs
                ]
    prt i (BindC cxt sig parms rhs) =
        prPrec i 0 $
            concatD
                [ doc . showString $ "{" ++ intercalate ", " (map (\(x, _) -> printTree x ++ "^") cxt) ++ "}" ++ printTree sig
                , prt i parms
                , doc $ showString "="
                , prt i rhs
                ]

instance Print [Bind] where
    prt _ []       = concatD []
    prt i [x]      = concatD [prt i x]
    prt i (x : xs) = concatD [prt i x, doc (showString ";"), prt i xs]

instance Print Exp where
    prt i = \case
        EVar lident -> prPrec i 3 (concatD [prt 0 lident])
        EVarC as lident -> doc . showString
                               $ "{" ++ intercalate ", " (map go as) ++ "}" ++ printTree lident
                where
              go (x, _) = printTree x ++ "^=" ++ printTree (EVar x)
        EInj uident -> prPrec i 3 (concatD [prt 0 uident])
        ELit lit -> prPrec i 3 (concatD [prt 0 lit])
        EApp exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, prt 3 exp2])
        EAdd exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "+"), prt 2 exp2])
        ELet lident exp1 exp2 -> prPrec i 0 (concatD [doc (showString "let"), prt 0 lident, doc (showString "="), prt 0 exp1 , doc (showString "in"), prt 0 exp2])
        ECase exp branchs -> prPrec i 0 (concatD [doc (showString "case"), prt 0 exp, doc (showString "of"), doc (showString "{"), prt 0 branchs, doc (showString "}")])


instance Print Branch where
    prt i (Branch (pattern_, t) exp) = prPrec i 0 (concatD [doc (showString "("), prt 0 pattern_, doc (showString " : "), prt 0 t, doc (showString ")"), doc (showString "=>"), prt 0 exp])

instance Print [Branch] where
    prt _ []       = concatD []
    prt _ [x]      = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print Def where
    prt i = \case
        DBind bind  -> prPrec i 0 (concatD [prt 0 bind])
        DData data_ -> prPrec i 0 (concatD [prt 0 data_])

instance Print Data where
    prt i = \case
        Data type_ injs -> prPrec i 0 (concatD [doc (showString "data"), prt 0 type_, doc (showString "where"), doc (showString "{"), prt 0 injs, doc (showString "}")])

instance Print Inj where
    prt i = \case
        Inj uident type_ -> prt i (uident, type_)

instance Print [Inj] where
    prt _ [] = concatD []
    prt i [x] = prt i x
    prt i (x : xs) = prPrec i 0 $ concatD [prt i x, doc $ showString "\n  ", prt i xs]

instance Print Pattern where
    prt i = \case
        PVar name -> prPrec i 1 (concatD [prt 0 name])
        PLit lit -> prPrec i 1 (concatD [prt 0 lit])
        PCatch -> prPrec i 1 (concatD [doc (showString "_")])
        PEnum name -> prPrec i 1 (concatD [prt 0 name])
        PInj uident patterns -> prPrec i 0 (concatD [prt 0 uident, prt 1 patterns])

instance Print [Def] where
    prt _ []       = concatD []
    prt _ [x]      = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

pattern DBind' id vars expt = DBind (Bind id vars expt)
pattern DData' typ injs = DData (Data typ injs)

