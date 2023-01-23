{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module TypeChecker where

import Grammar.Abs
import Grammar.ErrM
import Data.Kind qualified as T
import Data.String qualified
import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad.Reader
import Control.Monad.Except

newtype Env = Env { signature :: Map Ident CType  }

type Check a = ReaderT Env Err a

initEnv :: Env
initEnv = Env { signature = mempty }

run :: Check a -> Either String a
run = flip runReaderT initEnv

checkProg :: Program -> Check Program
checkProg (Program ds) = Program <$> mapM checkDef ds
    
checkDef :: Def -> Check Def
checkDef = \case
    (DExp n1 TInt n2 params e) -> undefined
    (DExp n1 (TPol (Ident t)) n2 params e) -> undefined
    (DExp n1 ts n2 params e) -> undefined

class Typecheck a where
    checkExp :: Exp -> Check (CExp a)

instance Typecheck Int where
    checkExp = \case
        EInt i -> pure $ CInt (fromIntegral i)
        EAdd e1 e2 -> do
            e1' <- checkExp @Int e1
            e2' <- checkExp @Int e2
            return $ CAdd e1' e2'
        EId (Ident i) -> asks (lookupSig (Ident i)) >>= liftEither >>= \case
            TCInt -> pure (CId (CIdent i))
            _     -> throwError $ "Unbound variable " <> show i

data CExp :: T.Type -> T.Type where
    CId :: CIdent -> CExp a
    CInt :: Int -> CExp Int
    CAdd :: Num a => CExp a -> CExp a -> CExp a

instance Show (CExp a) where
    show = \case
        CId (CIdent a) -> show a
        CInt i -> show i
        CAdd e1 e2 -> show e1 <> " + " <> show e2

data CDef a = CDef CIdent CType CIdent [CIdent] (CExp a)
    deriving Show

newtype CProgram = CProgram [Def]

data CType = TCInt | TCPol Ident | TCFun Type Type
  deriving (Eq, Ord, Show, Read)

newtype CIdent = CIdent String
  deriving (Eq, Ord, Show, Read, Data.String.IsString)

lookupSig :: Ident -> Env -> Err CType
lookupSig i (Env m) = case Map.lookup i m of
                  Nothing -> throwError $ "Unbound variable: " <> show i
                  Just x -> pure x


