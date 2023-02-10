module Compiler.Compiler where

import           Compiler.StandardLLVMLibrary (standardLLVMLibrary)
import           Control.Monad.State          (State, execState, gets, modify)
import           Data.Set                     as Set
import           Grammar.Abs                  (Def (..), Exp (..), Ident (..)
                                              , Program (..), Type (..))
import           Grammar.Print                (printTree)
import           Compiler.LLVMIr              (LLVMIr(..), Value(..)
                                              , printLLVMIr, LLVMType(..))

-- | The record used as the code generator state
data CodeGenerator = CodeGenerator
                   { instructions  :: [LLVMIr]
                   , methods       :: [Ident]
                   , block         :: Set Ident
                   , variableCount :: Integer }
type CompilerState = State CodeGenerator ()

-- | An empty instance of CodeGenerator
defaultCodeGenerator :: CodeGenerator
defaultCodeGenerator = CodeGenerator
                    { instructions = []
                    , methods = []
                    , block = Set.empty
                    , variableCount = 0 }

-- | Adds a instruction to the CodeGenerator state
emit :: LLVMIr -> CompilerState
emit l = modify (\t -> t {instructions = instructions t ++ [l]})

-- | Increases the variable counter in the Codegenerator state
increaseVarCount :: CompilerState
increaseVarCount = modify (\t -> t {variableCount = variableCount t + 1})

compile :: Program -> IO ()
compile (Program prg) = do
    let s = defaultCodeGenerator {instructions = [
        Comment (show $ printTree (Program prg)),
        UnsafeRaw $ standardLLVMLibrary <> "\n"
    ]}
    let fin = execState (goDef prg) s
    let ins = instructions fin
    putStrLn $ concatMap printLLVMIr ins
    where
        mainContent var =
                [ SetVariable (Ident "print_res")
                , Call (Array 21 I8) (Ident "i64ToString") [(I64, VIdent $ Ident $ show var)]
                , SetVariable (Ident "print_ptr"), Alloca (Array 21 I8)
                , Store (Array 21 I8) (Ident "print_res") (Ref (Array 21 I8)) (Ident "print_ptr")
                , SetVariable (Ident "printable"), Bitcast (Ref (Array 21 I8)) (Ident "print_ptr") (Ref I8)
                , Call I32 (Ident "puts") [(Ref I8, VIdent (Ident "printable"))]
                , Ret I64 (VInteger 0)
                ]

        goDef :: [Def] -> CompilerState
        goDef [] = return ()
        goDef (DExp id@(Ident str) t _id2 args exp : xs) = do
            let (rt, argTypes) = flattenFuncType t
            emit $ Comment $ show str <> ": " <> show (rt, argTypes)
            emit $ Define rt id (zip argTypes args) -- //TODO parse args
            go exp
            varNum <- gets variableCount
            if str == "main" then mapM_ emit (mainContent varNum)
                             else emit $ Ret rt (VIdent (Ident (show varNum)))
            emit DefineEnd
            modify (\s -> s {variableCount = 0})
            goDef xs

        go :: Exp -> CompilerState
        go (EInt int)    = emitInt int
        go (EAdd e1 e2)  = emitAdd e1 e2
        go (ESub e1 e2)  = emitSub e1 e2
        go (EMul e1 e2)  = emitMul e1 e2
        go (EDiv e1 e2)  = emitDiv e1 e2
        go (EMod e1 e2)  = emitMod e1 e2
        go (EId  id)     = emitArg id
        go (EApp e1 e2)  = emitApp e1 e2
        go (EAbs id t e) = emitAbs id t e

        --- aux functions ---
        emitAbs :: Ident -> Type -> Exp -> CompilerState
        emitAbs id t e = do
            emit $ Comment $ concat [ "EAbs (", show id, ", ", show t, ", "
                                    , show e, ") is not implemented!"]

        emitApp :: Exp -> Exp -> CompilerState
        emitApp e1 e2 = do
            emit $ Comment $ concat [ "EApp (", show e1, ", ", show e2
                                    , ") is not implemented!"]

        emitArg :: Ident -> CompilerState
        emitArg id = do
            -- !!this should never happen!!
            increaseVarCount
            varCount <- gets variableCount
            emit $ SetVariable (Ident $ show varCount)
            emit $ Add I64 (VIdent id) (VInteger 0)

        emitInt :: Integer -> CompilerState
        emitInt i = do
            -- !!this should never happen!!
            increaseVarCount
            varCount <- gets variableCount
            emit $ SetVariable $ Ident (show varCount)
            emit $ Add I64 (VInteger i) (VInteger 0)

        emitAdd :: Exp -> Exp -> CompilerState
        emitAdd e1 e2 = do
            (v1,v2) <- binExprToValues e1 e2
            increaseVarCount
            v <- gets variableCount
            emit $ SetVariable $ Ident $ show v
            emit $ Add I64 v1 v2

        emitMul :: Exp -> Exp -> CompilerState
        emitMul e1 e2 = do
            (v1,v2) <- binExprToValues e1 e2
            increaseVarCount
            v <- gets variableCount
            emit $ SetVariable $ Ident $ show v
            emit $ Mul I64 v1 v2

        emitMod :: Exp -> Exp -> CompilerState
        emitMod e1 e2 = do
            -- `let m a b = rem (abs $ b + a) b`
            (v1,v2) <- binExprToValues e1 e2
            increaseVarCount
            vadd <- gets variableCount
            emit $ SetVariable $ Ident $ show vadd
            emit $ Add I64 v1 v2

            increaseVarCount
            vabs <- gets variableCount
            emit $ SetVariable $ Ident $ show vabs
            emit $ Call I64 (Ident "llvm.abs.i64")
                [ (I64, VIdent (Ident $ show vadd))
                , (I1, VInteger 1)
                ]
            increaseVarCount
            v <- gets variableCount
            emit $ SetVariable $ Ident $ show v
            emit $ Srem I64 (VIdent (Ident $ show vabs)) v2

        emitDiv :: Exp -> Exp -> CompilerState
        emitDiv e1 e2 = do
            (v1,v2) <- binExprToValues e1 e2
            increaseVarCount
            v <- gets variableCount
            emit $ SetVariable $ Ident $ show v
            emit $ Div I64 v1 v2

        emitSub :: Exp -> Exp -> CompilerState
        emitSub e1 e2 = do
            (v1,v2) <- binExprToValues e1 e2
            increaseVarCount
            v <- gets variableCount
            emit $ SetVariable $ Ident $ show v
            emit $ Sub I64 v1 v2

        exprToValue :: Exp -> State CodeGenerator Value
        exprToValue (EInt i) = return $ VInteger i
        exprToValue (EId i)  = return $ VIdent i
        exprToValue e        = do
            go e
            v <- gets variableCount
            return $ VIdent $ Ident $ show v

        binExprToValues :: Exp -> Exp -> State CodeGenerator (Value, Value)
        binExprToValues e1 e2 = do
            v1 <- exprToValue e1
            v2 <- exprToValue e2
            return (v1,v2)


-- | A pretty nasty function to flatten out function types,
--   as they are currently represented by a recursive data type.
flattenFuncType :: Type -> (LLVMType, [LLVMType])
flattenFuncType xs = do
    let res = go xs
    (last res, init res)
    where
        go TInt         = [I64]
        go (TPol id)    = [CustomType id]
        go (TFun t1 t2) = go t1 ++ go t2
