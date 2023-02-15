{-# LANGUAGE LambdaCase #-}

module Renamer (module Renamer) where

import           Data.List   (mapAccumL, unzip4, zipWith4)
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe  (fromMaybe)
import           Grammar.Abs


-- | Rename all supercombinators and variables
rename :: Program -> Program
rename (Program sc) = Program $ map (renameSc 0) sc
  where
    renameSc i (Bind n t _ xs e) = Bind n t n xs' e'
      where
        (i1, xs', env) = newNames i xs
        e'             = snd $ renameExp env i1 e

renameExp :: Map Ident Ident -> Int -> Exp -> (Int, Exp)
renameExp env i = \case

  EId  n     -> (i, EId . fromMaybe n $ Map.lookup n env)

  EInt i1    -> (i, EInt i1)

  EApp e1 e2 -> (i2, EApp e1' e2')
    where
      (i1, e1') = renameExp env i e1
      (i2, e2') = renameExp env i1 e2

  EAdd e1 e2 -> (i2, EAdd e1' e2')
    where
      (i1, e1') = renameExp env i e1
      (i2, e2') = renameExp env i1 e2

  ELet bs e  -> (i3, ELet (zipWith4 mkBind names' types pars' es') e')
    where
      mkBind name t              = Bind name t name
      (i1, e')                   = renameExp e_env i e
      (names, types, pars, rhss) = fromBinders bs
      (i2, names', env')         = newNames i1 (names ++ concat pars)
      pars'                      = (map . map) renamePar pars
      e_env                      = Map.union env' env
      (i3, es')                  = mapAccumL (renameExp e_env) i2 rhss

      renamePar p = case Map.lookup p env' of
                     Just p' -> p'
                     Nothing -> error ("Can't find name for " ++ show p)


  EAbs par t e  -> (i2, EAbs par' t e')
    where
      (i1, par', env') = newName par
      (i2, e')         = renameExp (Map.union env' env ) i1 e

  EAnn e t -> (i1, EAnn e' t)
    where
      (i1, e') = renameExp env i e


newName :: Ident -> (Int, Ident, Map Ident Ident)
newName old_name = (i, head names, env)
  where (i, names, env) = newNames 1 [old_name]

newNames :: Int -> [Ident] -> (Int, [Ident], Map Ident Ident)
newNames i old_names = (i', new_names, env)
  where
    (i', new_names) = getNames i old_names
    env = Map.fromList $ zip old_names new_names

getNames :: Int -> [Ident] -> (Int, [Ident])
getNames i ns = (i + length ss, zipWith makeName ss [i..])
  where
    ss = map (\(Ident s) -> s) ns

makeName :: String -> Int -> Ident
makeName prefix i = Ident (prefix ++ "_" ++ show i)


fromBinders :: [Bind] -> ([Ident], [Type], [[Ident]], [Exp])
fromBinders bs = unzip4 [ (name, t, parms, rhs) | Bind name t _ parms rhs <- bs ]
