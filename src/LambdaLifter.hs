{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}


module LambdaLifter (lambdaLift, freeVars, abstract) where

import           Data.List        (mapAccumL)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (fromJust)
import           Data.Set         (Set, (\\))
import qualified Data.Set         as Set
import           Data.Tuple.Extra (uncurry3)
import           Grammar.Abs
import           Prelude          hiding (exp)

pattern Sc :: Ident -> [Ident] -> Exp -> ScDef
pattern Sc n xs e = ScDef (Bind n xs e)



lambdaLift :: Program -> Program
lambdaLift = rename . abstract . freeVars


-- Annotate free variables

freeVars :: Program -> AnnProgram
freeVars (Program ds) = [ (n, xs, freeVarsExp (Set.fromList xs) e)
                        | Sc n xs e <- ds
                        ]


freeVarsExp :: Set Ident -> Exp -> AnnExp
freeVarsExp lv = \case

  EId n | Set.member n lv -> (Set.singleton n, AId n)
        | otherwise       -> (mempty, AId n)

  EInt i     -> (mempty, AInt i)

  EApp e1 e2 -> (Set.union (freeVarsOf e1') (freeVarsOf e2'), AApp e1' e2')
    where e1' = freeVarsExp lv e1
          e2' = freeVarsExp lv e2

  EAdd e1 e2 -> (Set.union (freeVarsOf e1') (freeVarsOf e2'), AAdd e1' e2')
    where e1' = freeVarsExp lv e1
          e2' = freeVarsExp lv e2

  EAbs n  e  -> (Set.delete n $ freeVarsOf e', AAbs n e')
    where e'  = freeVarsExp (Set.insert n lv) e

  ELet bs e  -> (Set.union bsFree eFree, ALet bs' e')
    where
      bsFree       = freeInValues \\ nsSet
      eFree        = freeVarsOf e' \\ nsSet
      bs'          = zipWith3 ABind ns xs es'
      e'           = freeVarsExp e_lv e
      (ns, xs, es) = fromBinders bs
      nsSet        = Set.fromList ns
      e_lv         = Set.union lv nsSet
      es'          = map (freeVarsExp e_lv) es
      freeInValues = foldr1 Set.union (map freeVarsOf es')



freeVarsOf :: AnnExp -> Set Ident
freeVarsOf = fst

fromBinders :: [Bind] -> ([Ident], [[Ident]], [Exp])
fromBinders bs = unzip3 [ (n, xs, e) | Bind n xs e <- bs ]

-- Lift lambda expression into let with binder "sc"

abstract :: AnnProgram -> Program
abstract p = Program
  [ Sc sc_name xs $ abstractExp rhs
  | (sc_name, xs, rhs) <- p
  ]


abstractExp :: AnnExp -> Exp
abstractExp (free, exp) = case exp of
  AId  n     -> EId n
  AInt i     -> EInt i
  AApp e1 e2 -> EApp (abstractExp e1) (abstractExp e2)
  AAdd e1 e2 -> EAdd (abstractExp e1) (abstractExp e2)
  ALet bs e  -> ELet [Bind n xs (abstractExp e1) | ABind n xs e1 <- bs ] $ abstractExp e
  AAbs n  e  -> foldl EApp sc (map EId fvList)
    where
      fvList = Set.toList free
      bind = Bind "sc" [] e'
      e' = foldr EAbs (abstractExp e) (fvList ++ [n])
      sc = ELet [bind] (EId (Ident "sc"))

-- rename pass

rename :: Program -> Program
rename (Program ds) = Program $ map (uncurry3 Sc) tuples
  where
    tuples = snd (mapAccumL renameSc 0 ds)
    renameSc i (Sc n xs e) = (i2, (n, xs', e'))
      where
        (i1, xs', env) = newNames i xs
        (i2, e')       = renameExp env i1 e

renameExp :: Map Ident Ident -> Int -> Exp -> (Int, Exp)
renameExp env i = \case


  EId  n     -> (i, maybe (error "no") EId $ Map.lookup n env)


  EInt i1    -> (i, EInt i1)

  EApp e1 e2 -> (i2, EApp e1' e2')
    where
      (i1, e1') = renameExp env i e1
      (i2, e2') = renameExp env i1 e2

  EAdd e1 e2 -> (i2, EAdd e1' e2')
    where
      (i1, e1') = renameExp env i e1
      (i2, e2') = renameExp env i1 e2

  ELet bs e  -> (i3, ELet (zipWith3 Bind ns' xs es') e')
    where
      (i1, e')        = renameExp e_env i e
      (ns, xs, es)    = fromBinders bs
      (i2, ns', env') = newNames i1 ns
      e_env           = Map.union env' env
      (i3, es')       = mapAccumL (renameExp e_env) i2 es


  EAbs n  e  -> (i2, EAbs (head ns) e')
    where
      (i1, ns, env') = newNames i [n]
      (i2, e')       = renameExp (Map.union env' env ) i1 e


newNames :: Int -> [Ident] -> (Int, [Ident], Map Ident Ident)
newNames i old_names = (i', new_names, env)
  where
    (i', new_names) = getNames i old_names
    env = Map.fromList $ zip old_names new_names


getName :: Int -> Ident -> (Int, Ident)
getName i (Ident s) = (i + 1, makeName s i)

getNames :: Int -> [Ident] -> (Int, [Ident])
getNames i ns = (i + length ss, zipWith makeName ss [i..])
  where
    ss = map (\(Ident s) -> s) ns

makeName :: String -> Int -> Ident
makeName prefix i = Ident (prefix ++ "_" ++ show i)

-- Annotated AST

type AnnProgram = [(Ident, [Ident], AnnExp)]
type AnnExp = (Set Ident, AnnExp')

data ABind = ABind Ident [Ident] AnnExp deriving Show

data AnnExp' = AId Ident
             | AInt Integer
             | AApp AnnExp  AnnExp
             | AAdd AnnExp  AnnExp
             | AAbs Ident   AnnExp
             | ALet [ABind] AnnExp
             deriving Show
