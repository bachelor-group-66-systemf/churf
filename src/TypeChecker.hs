{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TypeChecker (typecheck) where

import Grammar.Abs
import Grammar.ErrM ( Err )
import NewAbs
import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad.Reader
import Control.Monad.Except
import Data.List (isPrefixOf)
import Control.Applicative ((<|>))

type Check a = ReaderT Context Err a

data Context = Ctx { sig :: Map Ident Type
                   , env :: [Map Ident Type]
                   }

initEnv :: Context
initEnv = Ctx { sig = mempty
              , env = mempty
              }

run :: Check a -> Either String a
run = flip runReaderT initEnv

typecheck :: Program -> Err Program
typecheck prg = case run $ checkProg prg of
                  Left err -> fail err
                  Right _ -> pure prg


checkProg :: Program -> Check CProgram
checkProg (Program ds) = undefined
    
checkDef :: Def -> Check CDef
checkDef (DExp i1 TInt i2 args e) = undefined
checkDef (DExp i1 (TPol i) i2 args e) = undefined
checkDef (DExp i1 (TFun xs) i2 args e) = do
        when (i1 /= i2) (fail $ "Mismatched names: " <> show i1 <> " != " <> show i2)
        case compare (length xs - 1) (length args) of
          LT -> fail $ "Too many arguments, got " <> show (length args) <> " expected " <> show (length xs)
          _ -> do
              let vars = Map.fromList $ zip args xs
              e' <- local (\r -> r { env =  [vars] }) (checkExp e)
              return $ CDef i1 (TFun xs) i2 args e'

checkExp :: Exp -> Check CExp
checkExp = \case

    EInt i -> pure $ CInt TInt (fromIntegral i)

    EAdd e1 e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let t1 = getType e1'
        let t2 = getType e2'
        when (t1 /= t2) (fail $ "Different types occured, got " <> show t1 <> " and " <> show t2)
        return $ CAdd t1 e1' e2'

    EId i -> do
        asks (lookupEnv i) >>= \case
            Right t -> return $ CId t i
            Left _ -> asks (lookupSig i) >>= \case
                Right t -> return $ CId t i
                Left x -> fail x

    EAbs i t e -> do
        e' <- local (\r -> r { env = Map.singleton i t : r.env }) (checkExp e)
        return $ CAbs (TFun [t, getType e']) i t e'

    EApp e1 e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let retT = applyType (getType e1') (getType e2')
        case retT of
          Left x -> fail x
          Right t -> return $ CApp t e1' e2'

lookupSig :: Ident -> Context -> Err Type
lookupSig i (Ctx s _) = case Map.lookup i s of
                  Nothing -> throwError $ "Undefined function: " <> show i
                  Just x -> pure x

lookupEnv :: Ident -> Context -> Err Type
lookupEnv i (Ctx _ []) = throwError $ "Unbound variable: " <> show i
lookupEnv i (Ctx s (e:es)) = case Map.lookup i e of
                  Nothing -> lookupEnv i (Ctx s es)
                  Just x -> pure x


applyType :: Type -> Type -> Err Type
applyType (TFun (x:xs)) t = case t of
    (TFun ys) -> if ys `isPrefixOf` (x:xs)
                         then return . TFun $ drop (length ys) (x:xs)
                         else fail $ "Mismatched types, expected " <> show x <> " got " <> show TInt
applyType t1 t2 = fail $ "Can not apply " <> show t1 <> " to " <> show t2

class ExtractType a where
    getType :: a -> Type

instance ExtractType CExp where
    getType = \case
        CId t _ -> t
        CInt t _ -> t
        CAdd t _ _ -> t
        CAbs t _ _ _ -> t
        CApp t _ _ -> t

-- | λx : Int. x + 3 + 5
customLambda1 :: Exp
customLambda1 = EAbs (Ident "x") TInt (EAdd (EId (Ident "x")) (EAdd (EInt 3) (EInt 5)))

customLambda2 :: Exp
customLambda2 = EAbs (Ident "x") (TFun [TInt, TInt]) (EId (Ident "f"))

-- | main : Int 
--   main = λx : Int. x + 3 + 5
customPrg1 :: Program
customPrg1 = Program [DExp (Ident "main") TInt (Ident "main") [] customLambda1]

-- | main : Int -> Int
--   main = λx : Int. x + 3 + 5
customPrg2 :: Program
customPrg2 = Program [DExp (Ident "main") (TFun [TInt, TInt]) (Ident "main") [] customLambda2]
