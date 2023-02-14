{-# LANGUAGE LambdaCase, OverloadedRecordDot, OverloadedStrings #-}

module Renamer.Renamer (rename) where

import Renamer.RenamerIr
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Identity (Identity, runIdentity)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Renamer.RenamerIr
import qualified Grammar.Abs as Old

type Rename = StateT Ctx (ExceptT Error Identity)

data Ctx = Ctx { count :: Integer
               , sig :: Set Ident
               , env :: Map Ident Integer}

run :: Rename a -> Either Error a
run = runIdentity . runExceptT . flip evalStateT initCtx

initCtx :: Ctx
initCtx = Ctx { count = 0
              , sig = mempty
              , env = mempty }

rename :: Old.Program -> Either Error RProgram
rename = run . renamePrg

renamePrg :: Old.Program -> Rename RProgram
renamePrg (Old.Program xs) = do
    xs' <- mapM renameBind xs
    return $ RProgram xs'

renameBind :: Old.Bind -> Rename RBind
renameBind (Old.Bind i args e) = do
    insertSig i
    e' <- renameExp (makeLambda (reverse args) e)
    return $ RBind i e'
  where 
    makeLambda :: [Ident] -> Old.Exp -> Old.Exp
    makeLambda [] e = e
    makeLambda (x:xs) e = makeLambda xs (Old.EAbs x e)

renameExp :: Old.Exp -> Rename RExp
renameExp = \case

    Old.EId i -> do
        st <- get
        case M.lookup i st.env of
            Just n -> return $ RBound n i
            Nothing -> case S.member i st.sig of
                         True -> return $ RFree i
                         False -> throwError $ UnboundVar (show i)

    Old.EConst c -> return $ RConst c

    Old.EAnn e t -> flip RAnn t <$> renameExp e

    Old.EApp e1 e2 -> RApp <$> renameExp e1 <*> renameExp e2

    Old.EAdd e1 e2 -> RAdd <$> renameExp e1 <*> renameExp e2

    -- Convert let-expressions to lambdas
    Old.ELet i e1 e2 -> renameExp (Old.EApp (Old.EAbs i e2) e1)

    Old.EAbs i e -> do
        n <- cnt
        ctx <- get
        insertEnv i n
        re <- renameExp e
        return $ RAbs n i re

-- | Get current count and increase it by one
cnt :: Rename Integer
cnt = do
    st <- get
    put (Ctx { count = succ st.count
             , sig = st.sig
             , env = st.env })
    return st.count
        
insertEnv :: Ident -> Integer -> Rename ()
insertEnv i n = do
    c <- get
    put ( Ctx { env = M.insert i n c.env , sig = c.sig , count = c.count} )

insertSig :: Ident -> Rename ()
insertSig i = do
    c <- get
    put ( Ctx { sig = S.insert i c.sig , env = c.env , count = c.count } )

data Error = UnboundVar String

instance Show Error where
    show (UnboundVar str) = "Unbound variable: " <> str
