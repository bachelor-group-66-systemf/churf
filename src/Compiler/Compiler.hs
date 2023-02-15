module Compiler.Compiler (compile) where

import Compiler.LLVMIr (
    LLVMIr (..),
    LLVMType (..),
    LLVMValue (..),
    llvmIrToString,
 )
import Control.Monad.State (StateT, execStateT, gets, modify)
import Data.Map (Map)
import Data.Map qualified as Map
import Grammar.Abs (
    Bind (..),
    Exp (..),
    Ident (..),
    Program (..),
 )
import Grammar.ErrM (Err)
import Grammar.Print (printTree)

-- | The record used as the code generator state
data CodeGenerator = CodeGenerator
    { instructions :: [LLVMIr]
    , functions :: Map Ident FunctionInfo
    , variableCount :: Integer
    }

-- | A state type synonym
type CompilerState a = StateT CodeGenerator Err a

data FunctionInfo = FunctionInfo
    { numArgs :: Int
    , arguments :: [Ident]
    }

-- | Adds a instruction to the CodeGenerator state
emit :: LLVMIr -> CompilerState ()
emit l = modify (\t -> t{instructions = instructions t ++ [l]})

-- | Increases the variable counter in the CodeGenerator state
increaseVarCount :: CompilerState ()
increaseVarCount = modify (\t -> t{variableCount = variableCount t + 1})

-- | Returns the variable count from the CodeGenerator state
getVarCount :: CompilerState Integer
getVarCount = gets variableCount

-- | Increases the variable count and returns it from the CodeGenerator state
getNewVar :: CompilerState Integer
getNewVar = increaseVarCount >> getVarCount

{- | Produces a map of functions infos from a list of binds,
  which contains useful data for code generation.
-}
getFunctions :: [Bind] -> Map Ident FunctionInfo
getFunctions xs =
    Map.fromList $
        map
            ( \(Bind id args _) ->
                ( id
                , FunctionInfo
                    { numArgs = length args
                    , arguments = args
                    }
                )
            )
            xs

{- | Compiles an AST and produces a LLVM Ir string.
  An easy way to actually "compile" this output is to
  Simply pipe it to LLI
-}
compile :: Program -> Err String
compile (Program prg) = do
    let s =
            CodeGenerator
                { instructions = defaultStart
                , functions = getFunctions prg
                , variableCount = 0
                }
    ins <- instructions <$> execStateT (goDef prg) s
    pure $ llvmIrToString ins
  where
    mainContent :: LLVMValue -> [LLVMIr]
    mainContent var =
        [ UnsafeRaw $
            "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef " <> show var <> ")\n"
        , Ret I64 (VInteger 0)
        ]

    defaultStart :: [LLVMIr]
    defaultStart =
        [ Comment (show $ printTree (Program prg))
        , UnsafeRaw "@.str = private unnamed_addr constant [3 x i8] c\"%i\n\", align 1\n"
        , UnsafeRaw "declare i32 @printf(ptr noalias nocapture, ...)\n"
        ]

    goDef :: [Bind] -> CompilerState ()
    goDef [] = return ()
    goDef (Bind id@(Ident str) args exp : xs) = do
        emit $ UnsafeRaw "\n"
        emit $ Comment $ show str <> ": " <> show exp
        emit $ Define I64 id (map (I64,) args)
        functionBody <- exprToValue exp
        if str == "main"
            then mapM_ emit (mainContent functionBody)
            else emit $ Ret I64 functionBody
        emit DefineEnd
        modify (\s -> s{variableCount = 0})
        goDef xs

    go :: Exp -> CompilerState ()
    go (EInt int) = emitInt int
    go (EAdd e1 e2) = emitAdd e1 e2
    -- go (ESub e1 e2)  = emitSub e1 e2
    -- go (EMul e1 e2)  = emitMul e1 e2
    -- go (EDiv e1 e2)  = emitDiv e1 e2
    -- go (EMod e1 e2)  = emitMod e1 e2
    go (EId id) = emitIdent id
    go (EApp e1 e2) = emitApp e1 e2
    go (EAbs id e) = emitAbs id e
    go (ELet xs e) = emitLet xs e

    --- aux functions ---
    emitAbs :: Ident -> Exp -> CompilerState ()
    emitAbs id e = do
        emit $
            Comment $
                concat
                    [ "EAbs ("
                    , show id
                    , ", "
                    , show I64
                    , ", "
                    , show e
                    , ") is not implemented!"
                    ]
    emitLet :: [Bind] -> Exp -> CompilerState ()
    emitLet xs e = do
        emit $
            Comment $
                concat
                    [ "ELet ("
                    , show xs
                    , " = "
                    , show e
                    , ") is not implemented!"
                    ]

    emitApp :: Exp -> Exp -> CompilerState ()
    emitApp e1 e2 = appEmitter e1 e2 []
      where
        appEmitter :: Exp -> Exp -> [Exp] -> CompilerState ()
        appEmitter e1 e2 stack = do
            let newStack = e2 : stack
            case e1 of
                EApp e1' e2' -> appEmitter e1' e2' newStack
                EId id -> do
                    args <- traverse exprToValue newStack
                    vs <- getNewVar
                    emit $ SetVariable (Ident $ show vs) (Call I64 id (map (I64,) args))
                x -> do
                    emit . Comment $ "The unspeakable happened: "
                    emit . Comment $ show x

    emitIdent :: Ident -> CompilerState ()
    emitIdent id = do
        -- !!this should never happen!!
        emit $ Comment "This should not have happened!"
        emit $ Variable id
        emit $ UnsafeRaw "\n"

    emitInt :: Integer -> CompilerState ()
    emitInt i = do
        -- !!this should never happen!!
        varCount <- getNewVar
        emit $ Comment "This should not have happened!"
        emit $ SetVariable (Ident (show varCount)) (Add I64 (VInteger i) (VInteger 0))

    emitAdd :: Exp -> Exp -> CompilerState ()
    emitAdd e1 e2 = do
        v1 <- exprToValue e1
        v2 <- exprToValue e2
        v <- getNewVar
        emit $ SetVariable (Ident $ show v) (Add I64 v1 v2)

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

    exprToValue :: Exp -> CompilerState LLVMValue
    exprToValue (EInt i) = return $ VInteger i
    exprToValue (EId id) = do
        funcs <- gets functions
        case Map.lookup id funcs of
            Just _ -> do
                vc <- getNewVar
                emit $ SetVariable (Ident $ show vc) (Call I64 id [])
                return $ VIdent (Ident $ show vc)
            Nothing -> return $ VIdent id
    exprToValue e = do
        go e
        v <- getVarCount
        return . VIdent . Ident $ show v
