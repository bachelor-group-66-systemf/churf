module Compiler.Compiler where

import           Compiler.LLVMIr     (LLVMIr (..), LLVMType (..), Value (..),
                                      llvmIrToString)
import           Control.Monad.State (StateT, execStateT, gets, modify)
import           Debug.Trace         (trace)
import           Grammar.Abs         (Bind (..), Exp (..), Ident (..),
                                      Program (..))
import           Grammar.ErrM        (Err)
import           Grammar.Print       (printTree)

-- | The record used as the code generator state
data CodeGenerator = CodeGenerator
                   { instructions  :: [LLVMIr]
                   , methods       :: [Ident]
                   , variableCount :: Integer }
type CompilerState a = StateT CodeGenerator Err a

-- | An empty instance of CodeGenerator
defaultCodeGenerator :: CodeGenerator
defaultCodeGenerator = CodeGenerator
                    { instructions = []
                    , methods = []
                    , variableCount = 0 }

-- | Adds a instruction to the CodeGenerator state
emit :: LLVMIr -> CompilerState ()
emit l = modify (\t -> t {instructions = instructions t ++ [l]})

-- | Increases the variable counter in the Codegenerator state
increaseVarCount :: CompilerState ()
increaseVarCount = modify (\t -> t {variableCount = variableCount t + 1})

compile :: Program -> Err String
compile (Program prg) = do
    let s = defaultCodeGenerator {instructions =
        [ Comment (show $ printTree (Program prg))
        , UnsafeRaw "@.str = private unnamed_addr constant [3 x i8] c\"%i\n\", align 1\n"
        , UnsafeRaw "declare i32 @printf(ptr noalias nocapture, ...)\n"
        -- , UnsafeRaw $ standardLLVMLibrary <> "\n"
        ]}
    fin <- execStateT (goDef prg) s
    let ins = instructions fin
    pure $ concatMap llvmIrToString ins
    where
        mainContent :: Integer -> [LLVMIr]
        mainContent var =
                [ UnsafeRaw $
                    "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef %" <> show var <> ")"
                , Ret I64 (VInteger 0)
                ]

        goDef :: [Bind] -> CompilerState ()
        goDef [] = return ()
        goDef (Bind id@(Ident str) args exp : xs) = do
            emit $ UnsafeRaw "\n"
            emit $ Comment $ show str <> ": " <> show exp
            emit $ Define I64 id (map (I64,) args) -- //TODO parse args
            case exp of
                EId id -> emit $ Ret  I64 (VIdent id)
                _      -> do
                    go exp
                    varNum <- gets variableCount
                    if str == "main" then mapM_ emit (mainContent varNum)
                                     else emit $ Ret I64 (VIdent (Ident (show varNum)))
            emit DefineEnd
            modify (\s -> s {variableCount = 0})
            goDef xs

        go :: Exp -> CompilerState ()
        go (EInt int)   = emitInt int
        go (EAdd e1 e2) = emitAdd e1 e2
        --go (ESub e1 e2)  = emitSub e1 e2
        --go (EMul e1 e2)  = emitMul e1 e2
        --go (EDiv e1 e2)  = emitDiv e1 e2
        --go (EMod e1 e2)  = emitMod e1 e2
        go (EId  id)    = emitIdent id
        go (EApp e1 e2) = emitApp e1 e2
        go (EAbs id e)  = emitAbs id e
        go (ELet xs e)  = emitLet xs e

        --- aux functions ---
        emitAbs :: Ident -> Exp -> CompilerState ()
        emitAbs id e = do
            emit $ Comment $ concat [ "EAbs (", show id, ", ", show I64, ", "
                                    , show e, ") is not implemented!"]
        emitLet :: [Bind] -> Exp -> CompilerState ()
        emitLet xs e = do
            emit $ Comment $ concat [ "ELet (", show xs, " = "
                                    , show e, ") is not implemented!"]

        emitApp :: Exp -> Exp -> CompilerState ()
        emitApp e1 e2 = appEmitter e1 e2 []
            where
                appEmitter :: Exp -> Exp -> [Exp] -> CompilerState ()
                appEmitter e1 e2 stack = do
                    let newStack =  e2 : stack
                    case e1 of
                        EApp e1' e2' -> appEmitter e1' e2' newStack
                        EId id  -> do
                            args <- traverse exprToValue newStack
                            increaseVarCount
                            vs <- gets variableCount
                            emit $ SetVariable (Ident $ show vs)
                            emit $ Call I64 id (map (I64,) args)
                        x -> do
                            emit . Comment $ "The unspeakable happened: "
                            emit . Comment $ show x

        --emitApp (EId id) e2 = do
        --    go e2
        --    vc <- gets variableCount
        --    increaseVarCount
        --    vc' <- gets variableCount
        --    emit $ SetVariable (Ident $ show vc')
        --    emit $ Call I64 id [(I64, VIdent (Ident $ show vc))]
        --emitApp e1 e2 = do
        --    go e2
        --    go e1

        emitIdent :: Ident -> CompilerState ()
        emitIdent id = do
            -- !!this should never happen!!
            -- increaseVarCount
            -- varCount <- gets variableCount
            -- emit $ SetVariable (Ident $ show varCount)
            -- emit $ Add I64 (VIdent id) (VInteger 0)
            emit $ Comment "This should not have happened!"
            emit $ Variable id
            emit $ UnsafeRaw "\n"

        emitInt :: Integer -> CompilerState ()
        emitInt i = do
            -- !!this should never happen!!
            increaseVarCount
            varCount <- gets variableCount
            emit $ Comment "This should not have happened!"
            emit $ SetVariable $ Ident (show varCount)
            emit $ Add I64 (VInteger i) (VInteger 0)

        emitAdd :: Exp -> Exp -> CompilerState ()
        emitAdd e1 e2 = do
            (v1,v2) <- binExprToValues e1 e2
            increaseVarCount
            v <- gets variableCount
            emit $ SetVariable $ Ident $ show v
            emit $ Add I64 v1 v2

        -- emitMul :: Exp -> Exp -> CompilerState ()
        -- emitMul e1 e2 = do
        --     (v1,v2) <- binExprToValues e1 e2
        --     increaseVarCount
        --     v <- gets variableCount
        --     emit $ SetVariable $ Ident $ show v
        --     emit $ Mul I64 v1 v2

        -- emitMod :: Exp -> Exp -> CompilerState ()
        -- emitMod e1 e2 = do
        --     -- `let m a b = rem (abs $ b + a) b`
        --     (v1,v2) <- binExprToValues e1 e2
        --     increaseVarCount
        --     vadd <- gets variableCount
        --     emit $ SetVariable $ Ident $ show vadd
        --     emit $ Add I64 v1 v2
        --
        --     increaseVarCount
        --     vabs <- gets variableCount
        --     emit $ SetVariable $ Ident $ show vabs
        --     emit $ Call I64 (Ident "llvm.abs.i64")
        --         [ (I64, VIdent (Ident $ show vadd))
        --         , (I1, VInteger 1)
        --         ]
        --     increaseVarCount
        --     v <- gets variableCount
        --     emit $ SetVariable $ Ident $ show v
        --     emit $ Srem I64 (VIdent (Ident $ show vabs)) v2

        -- emitDiv :: Exp -> Exp -> CompilerState ()
        -- emitDiv e1 e2 = do
        --     (v1,v2) <- binExprToValues e1 e2
        --     increaseVarCount
        --     v <- gets variableCount
        --     emit $ SetVariable $ Ident $ show v
        --     emit $ Div I64 v1 v2

        -- emitSub :: Exp -> Exp -> CompilerState ()
        -- emitSub e1 e2 = do
        --     (v1,v2) <- binExprToValues e1 e2
        --     increaseVarCount
        --     v <- gets variableCount
        --     emit $ SetVariable $ Ident $ show v
        --     emit $ Sub I64 v1 v2

        exprToValue :: Exp -> CompilerState Value
        exprToValue (EInt i) = return $ VInteger i
        exprToValue (EId i)  = return $ VIdent i
        exprToValue e        = do
            go e
            v <- gets variableCount
            return $ VIdent $ Ident $ show v

        binExprToValues :: Exp -> Exp -> CompilerState (Value, Value)
        binExprToValues e1 e2 = do
            v1 <- exprToValue e1
            v2 <- exprToValue e2
            return (v1,v2)

-- | A pretty nasty function to flatten out function types,
--   as they are currently represented by a recursive data type.
-- flattenFuncType :: Type -> (LLVMType, [LLVMType])
-- flattenFuncType xs = do
--     let res = go xs
--     (last res, init res)
--     where
--         go TInt         = [I64]
--         go (TPol id)    = [CustomType id]
--         go (TFun t1 t2) = go t1 ++ go t2

cTrace :: Show a => a -> a
cTrace a = trace (show a) a
