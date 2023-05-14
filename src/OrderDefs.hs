{-# LANGUAGE LambdaCase #-}

module OrderDefs where

import           Control.Monad.State (State, execState, get, modify, when)
import           Data.Function       (on)
import           Data.List           (find, partition, sortBy)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Grammar.Abs
import           Grammar.Print       (printTree)

orderDefs :: Program -> Program
orderDefs (Program defs) =
    Program $ ds ++ ss' ++ concatMap addSig (orderBinds bs)
  where
    addSig b
        | Just sig <- hasSig b = [sig, DBind b]
        | otherwise            = [DBind b]

    hasSig (Bind n _ _) = find (\(DSig (Sig n' _)) -> n' == n) ss

    (ss, ss') = partition hasBind [DSig s | DSig s <- defs]
    hasBind (DSig (Sig n _)) = any (\(Bind n' _ _) -> n' == n) bs

    bs = [ b | DBind b <- defs]
    ds = [ DData d | DData d <- defs]

orderBinds :: [Bind] -> [Bind]
orderBinds binds = sortBy (on compare countUniqueCalls) binds
  where
    bind_names = [n | Bind n _ _ <- binds]

    countUniqueCalls :: Bind -> Int
    countUniqueCalls b@BindS{} = error $ "Desugar failed to desugar bind correctly: " ++ printTree b
    countUniqueCalls (Bind n _ e) =
        Set.size $ execState (go e) (Set.singleton n)
      where
        go :: Exp -> State (Set LIdent) ()
        go exp =
            get >>= \called -> case exp of
                EVar x ->
                    when (Set.notMember x called && elem x bind_names) $
                        modify (Set.insert x)
                EApp e1 e2 -> on (>>) go e1 e2
                EAdd e1 e2 -> on (>>) go e1 e2
                ELet (Bind _ _ e) e' -> on (>>) go e e'
                EAbs _ e -> go e
                ECase e bs -> go e >> mapM_ (\(Branch _ e) -> go e) bs
                EAnn e _ -> go e
                EInj _ -> pure ()
                ELit _ -> pure ()
                e -> error $ "Desugar failed to desugar expression correctly: " ++ printTree e
