module Monomorphizer.DataTypeRemover (removeDataTypes) where
import qualified Monomorphizer.MorbIr as M1
import qualified Monomorphizer.MonomorphizerIr as M2
import TypeChecker.TypeCheckerIr (Ident (Ident))

removeDataTypes :: M1.Program -> M2.Program
removeDataTypes (M1.Program defs) = M2.Program (map pDef defs)

pDef :: M1.Def -> M2.Def
pDef (M1.DBind b) = M2.DBind (pBind b)
pDef (M1.DData d) = M2.DData (pData d)

pData :: M1.Data -> M2.Data
pData (M1.Data t cs) = M2.Data (pType t) (map pCons cs)

pCons :: M1.Inj -> M2.Inj
pCons (M1.Inj ident t) = M2.Inj ident (pType t)

pType :: M1.Type -> M2.Type
pType (M1.TLit ident) = M2.TLit ident
pType (M1.TFun t1 t2) = M2.TFun (pType t1) (pType t2)
pType (M1.TData (Ident str) args) = M2.TLit (Ident (str ++ show args))  -- This is the step

pBind :: M1.Bind -> M2.Bind
pBind (M1.Bind id argIds expt) = M2.Bind (pId id) (map pId argIds) (pExpT expt)

pId :: (Ident, M1.Type) -> (Ident, M2.Type)
pId (ident, t) = (ident, pType t)

pExpT :: M1.ExpT -> M2.ExpT
pExpT (exp, t) = (pExp exp, pType t)

pExp :: M1.Exp -> M2.Exp
pExp (M1.EVar ident)          = M2.EVar  ident
pExp (M1.ELit lit)            = M2.ELit  (pLit lit)
pExp (M1.ELet bind expt)      = M2.ELet  (pBind bind) (pExpT expt)
pExp (M1.EApp e1 e2)          = M2.EApp  (pExpT e1)   (pExpT e2)
pExp (M1.EAdd e1 e2)          = M2.EAdd  (pExpT e1)   (pExpT e2)
pExp (M1.ECase expT branches) = M2.ECase (pExpT expT) (map pBranch branches)

pBranch :: M1.Branch -> M2.Branch
pBranch (M1.Branch (patt, t) expt) = M2.Branch (pPattern patt, pType t) (pExpT expt)

pPattern :: M1.Pattern -> M2.Pattern
pPattern (M1.PVar id)          = M2.PVar (pId id)
pPattern (M1.PLit (lit, t))    = M2.PLit (pLit lit, pType t)
pPattern (M1.PInj ident patts) = M2.PInj ident (map pPattern patts)
pPattern M1.PCatch             = M2.PCatch
pPattern (M1.PEnum ident)      = M2.PEnum ident

pLit :: M1.Lit -> M2.Lit
pLit (M1.LInt v)  = M2.LInt v
pLit (M1.LChar c) = M2.LChar c

