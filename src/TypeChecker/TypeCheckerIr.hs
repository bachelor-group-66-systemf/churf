{-# LANGUAGE LambdaCase #-}

module TypeChecker.TypeCheckerIr where

-- import Control.Monad.Except
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Data.Functor.Identity (Identity)
-- import Data.Map (Map)
-- import Grammar.Abs (
--     Data (..),
--     Ident (..),
--     Init (..),
--     Literal (..),
--     Type (..),
--  )
-- import Grammar.Print
-- import Prelude
-- import Prelude qualified as C (Eq, Ord, Read, Show)

-- -- | A data type representing type variables
-- data Poly = Forall [Ident] Type
--     deriving (Show)

-- newtype Ctx = Ctx {vars :: Map Ident Poly}
--     deriving Show

-- data Env = Env
--     { count :: Int
--     , sigs :: Map Ident Type
--     , constructors :: Map Ident Type
--     } deriving Show

-- type Error = String
-- type Subst = Map Ident Type

-- type Infer = StateT Env (ReaderT Ctx (ExceptT Error Identity))

-- newtype Program = Program [Def]
--     deriving (C.Eq, C.Ord, C.Show, C.Read)

-- data Exp
--     = EId Id
--     | ELit Type Literal
--     | ELet Bind Exp
--     | EApp Type Exp Exp
--     | EAdd Type Exp Exp
--     | EAbs Type Id Exp
--     | ECase Type Exp [Inj]
--     deriving (C.Eq, C.Ord, C.Read, C.Show)

-- data Inj = Inj (Init, Type) Exp
--     deriving (C.Eq, C.Ord, C.Read, C.Show)

-- data Def = DBind Bind | DData Data
--     deriving (C.Eq, C.Ord, C.Read, C.Show)

-- type Id = (Ident, Type)

-- data Bind = Bind Id Exp
--     deriving (C.Eq, C.Ord, C.Show, C.Read)

-- instance Print [Def] where
--     prt _ [] = concatD []
--     prt _ (x : xs) = concatD [prt 0 x, doc (showString "\n"), prt 0 xs]

-- instance Print Def where
--     prt i (DBind bind) = prt i bind
--     prt i (DData d) = prt i d

-- instance Print Program where
--     prt i (Program sc) = prPrec i 0 $ prt 0 sc

-- instance Print Bind where
--     prt i (Bind (t, name) rhs) =
--         prPrec i 0 $
--             concatD
--                 [ prt 0 name
--                 , doc $ showString ":"
--                 , prt 0 t
--                 , doc $ showString "\n"
--                 , prt 0 name
--                 , doc $ showString "="
--                 , prt 0 rhs
--                 ]

-- instance Print [Bind] where
--     prt _ [] = concatD []
--     prt _ [x] = concatD [prt 0 x]
--     prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), doc (showString "\n"), prt 0 xs]

-- prtIdPs :: Int -> [Id] -> Doc
-- prtIdPs i = prPrec i 0 . concatD . map (prtIdP i)

-- prtId :: Int -> Id -> Doc
-- prtId i (name, t) =
--     prPrec i 0 $
--         concatD
--             [ prt 0 name
--             , doc $ showString ":"
--             , prt 0 t
--             ]

-- prtIdP :: Int -> Id -> Doc
-- prtIdP i (name, t) =
--     prPrec i 0 $
--         concatD
--             [ doc $ showString "("
--             , prt 0 name
--             , doc $ showString ":"
--             , prt 0 t
--             , doc $ showString ")"
--             ]

-- instance Print Exp where
--     prt i = \case
--         EId n -> prPrec i 3 $ concatD [prtId 0 n, doc $ showString "\n"]
--         ELit _ (LInt i1) -> prPrec i 3 $ concatD [prt 0 i1, doc $ showString "\n"]
--         ELet bs e ->
--             prPrec i 3 $
--                 concatD
--                     [ doc $ showString "let"
--                     , prt 0 bs
--                     , doc $ showString "in"
--                     , prt 0 e
--                     , doc $ showString "\n"
--                     ]
--         EApp _ e1 e2 ->
--             prPrec i 2 $
--                 concatD
--                     [ prt 2 e1
--                     , prt 3 e2
--                     ]
--         EAdd t e1 e2 ->
--             prPrec i 1 $
--                 concatD
--                     [ doc $ showString "@"
--                     , prt 0 t
--                     , prt 1 e1
--                     , doc $ showString "+"
--                     , prt 2 e2
--                     , doc $ showString "\n"
--                     ]
--         EAbs t n e ->
--             prPrec i 0 $
--                 concatD
--                     [ doc $ showString "@"
--                     , prt 0 t
--                     , doc $ showString "\\"
--                     , prtId 0 n
--                     , doc $ showString "."
--                     , prt 0 e
--                     , doc $ showString "\n"
--                     ]
--         ECase t exp injs ->
--             prPrec
--                 i
--                 0
--                 ( concatD
--                     [ doc (showString "case")
--                     , prt 0 exp
--                     , doc (showString "of")
--                     , doc (showString "{")
--                     , prt 0 injs
--                     , doc (showString "}")
--                     , doc (showString ":")
--                     , prt 0 t
--                     , doc $ showString "\n"
--                     ]
--                 )

-- instance Print Inj where
--     prt i = \case
--         Inj (init, t) exp -> prPrec i 0 (concatD [prt 0 init, doc (showString ":"), prt 0 t, doc (showString "=>"), prt 0 exp])

-- instance Print [Inj] where
--     prt _ [] = concatD []
--     prt _ [x] = concatD [prt 0 x]
--     prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]
