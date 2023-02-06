module Compiler.Compiler where

import           Compiler.StandardLLVMLibrary (standardLLVMLibrary)
import           Control.Applicative          (Applicative)
import           Control.Monad.Except         (Except, MonadError (throwError),
                                               liftEither)
import           Control.Monad.State
import           Data.Either.Combinators      (maybeToRight)
import           Data.List                    (intercalate)
import           Data.Set                     (Set)
import           Data.Set                     as Set
import           Grammar.Abs
import           Grammar.Par                  (myLexer, pProgram)
import           Grammar.Print                (printTree)
import           System.Exit                  (exitFailure)
--import LLVM.AST

compileFile :: String -> IO ()
compileFile file = do
    input <- readFile file
    case pProgram (myLexer input) of
        Left err -> do
            putStrLn "SYNTAX ERROR"
            putStrLn err
            exitFailure
        Right cor -> compile cor

data LLType = I1 | I8 | I32 | I64 | Ptr
            | Ref LLType | Array Integer LLType | CustomType Ident
instance Show LLType where
    show :: LLType -> String
    show t = case t of
        I1                    -> "i1"
        I8                    -> "i8"
        I32                   -> "i32"
        I64                   -> "i64"
        Ptr                   -> "ptr"
        Ref ty                -> show ty <> "*"
        Array n ty            -> concat ["[", show n, " x ", show ty, "]"]
        CustomType (Ident ty) -> ty

type Params = [(LLType, Ident)]
type Args = [(LLType, Value)]

data Value = VInteger Integer | VIdent Ident | VConstant String
instance Show Value where
    show :: Value -> String
    show v = case v of
        VInteger i       -> show i
        VIdent (Ident i) -> "%" <> i
        VConstant s      -> "c" <> show s

data CodeGenerator = CodeGenerator
                   { instructions  :: [LLVMIr]
                   , methods       :: [Ident]
                   , blocks        :: [Set Ident]
                   , variableCount :: Integer }
    deriving Show
defaultCodeGenerator :: CodeGenerator
defaultCodeGenerator = CodeGenerator
                    { instructions = []
                    , methods = []
                    , blocks = []
                    , variableCount = 0 }

data LLVMIr = Define LLType Ident Params
            | DefineEnd
            | Declare LLType Ident Params
            | Variable Ident
            | Add LLType Value Value
            | Sub LLType Value Value
            | Div LLType Value Value
            | Mul LLType Value Value
            | Srem LLType Value Value
            | Call LLType Ident Args
            | Alloca LLType
            | Store LLType Ident LLType Ident
            | Bitcast LLType Ident LLType
            | Ret LLType Value
            | UnsafeRaw String
            | Comment String
    deriving (Show)

printLLVMIr :: LLVMIr -> String
printLLVMIr (Define t (Ident i) params)           = concat ["define ", show t, " @", i, "(", intercalate "," (fmap (\(x,Ident y) -> unwords [show x, "%"<>y]) params),") {\n"]
printLLVMIr DefineEnd                             = "}\n"
printLLVMIr (Declare t (Ident i) params)          = undefined
printLLVMIr (Variable (Ident i))                  = concat ["%", i, " = "]
printLLVMIr (Add t v1 v2)                         = concat ["add ", show t, " ", show v1, ", ", show v2, "\n"]
printLLVMIr (Sub t v1 v2)                         = concat ["sub ", show t, " ", show v1, ", ", show v2, "\n"]
printLLVMIr (Div t v1 v2)                         = concat ["sdiv ", show t, " ", show v1, ", ", show v2, "\n"]
printLLVMIr (Mul t v1 v2)                         = concat ["mul ", show t, " ", show v1, ", ", show v2, "\n"]
printLLVMIr (Srem t v1 v2)                         = concat ["srem ", show t, " ", show v1, ", ", show v2, "\n"]
printLLVMIr (Call t (Ident i) arg)                = concat ["call ", show t, " @", i, "("
                                                           , intercalate ", " $ Prelude.map (\(x,y) -> show x <> " " <> show y) arg
                                                           , ")\n"]
printLLVMIr (Alloca t)                            = unwords ["alloca", show t, "\n"]
printLLVMIr (Store t1 (Ident id1) t2 (Ident id2)) = concat ["store ", show t1, " %", id1
                                                           , ", ", show t2, " %", id2, "\n"]
printLLVMIr (Bitcast t1 (Ident i) t2)             = concat ["bitcast ", show t1, " %", i, " to ", show t2, "\n"]
printLLVMIr (Ret t v)                             = concat ["ret ", show t, " ", show v, "\n"]
printLLVMIr (UnsafeRaw s)                         = s
printLLVMIr (Comment s)                           = "; " <> s <> "\n"

type CompilerState = State CodeGenerator ()

emit :: LLVMIr -> CompilerState
emit l = modify (\t -> t {instructions = instructions t ++ [l]})

increaseVarCount :: CompilerState
increaseVarCount = modify (\t -> t {variableCount = variableCount t + 1})

compile :: Program -> IO ()
compile (Program prg) = do
    --Asp
    let s = defaultCodeGenerator {instructions = [
        Comment (show $ printTree (Program prg)),
        UnsafeRaw $ standardLLVMLibrary <> "\n"
        --UnsafeRaw "declare i32 @puts(i8* nocapture) nounwind\n",
        --UnsafeRaw "declare [21 x i8] @i64ToString(i64)\n",
        --Define I32 (Ident "main") []
    ]}
    let fin = execState (goDef prg) s
    let ins = instructions fin
    putStrLn $ concatMap printLLVMIr ins
    where
        mainContent var =
                [ Variable (Ident "print_res")
                , Call (Array 21 I8) (Ident "i64ToString") [(I64, VIdent $ Ident $ show var)]
                , Variable (Ident "print_ptr"), Alloca (Array 21 I8)
                , Store (Array 21 I8) (Ident "print_res") (Ref (Array 21 I8)) (Ident "print_ptr")
                , Variable (Ident "printable"), Bitcast (Ref (Array 21 I8)) (Ident "print_ptr") (Ref I8)
                , Call I32 (Ident "puts") [(Ref I8, VIdent (Ident "printable"))]
                , Ret I64 (VInteger 0)
                ]

        goDef :: [Def] -> CompilerState
        goDef []                               = return ()
        goDef (DExp id@(Ident str) t _id2 args exp : xs) = do
            let (rt, argTypes) = flattenFuncType t
            emit $ Comment $ show (rt, argTypes)
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

        go (EId  id)     = emit $ Comment $ "EId (" <> show id <> ") is not implemented!"
        go (EApp e1 e2)  = emit $ Comment $ "EApp (" <> show e1 <> ", " <> show e2 <> ") is not implemented!"
        go (EAbs id t e) = emit $ Comment $ "EAbs (" <> show id <> ", " <> show t <> ", " <> show e <> ") is not implemented!"

        --- aux functions ---
        emitInt :: Integer -> CompilerState
        emitInt i = do
            -- ideally this case should not occur if the other
            -- emit operations are optimized
            increaseVarCount
            varCount <- gets variableCount
            emit $ Variable $ Ident (show varCount)
            emit $ Add I64 (VInteger i) (VInteger 0)

        emitAdd :: Exp -> Exp -> CompilerState
        emitAdd e1 e2 = do
            (v1,v2) <- binExprToValues e1 e2
            increaseVarCount
            v <- gets variableCount
            emit $ Variable $ Ident $ show v
            emit $ Add I64 v1 v2

        emitMul :: Exp -> Exp -> CompilerState
        emitMul e1 e2 = do
            (v1,v2) <- binExprToValues e1 e2
            increaseVarCount
            v <- gets variableCount
            emit $ Variable $ Ident $ show v
            emit $ Mul I64 v1 v2

        emitMod :: Exp -> Exp -> CompilerState
        emitMod e1 e2 = do
            -- `let m a b = rem (abs $ b + a) b`
            (v1,v2) <- binExprToValues e1 e2
            increaseVarCount
            vadd <- gets variableCount
            emit $ Variable $ Ident $ show vadd
            emit $ Add I64 v1 v2

            increaseVarCount
            vabs <- gets variableCount
            emit $ Variable $ Ident $ show vabs
            emit $ Call I64 (Ident "llvm.abs.i64")
                [ (I64, VIdent (Ident $ show vadd))
                , (I1, VInteger 1)
                ]
            increaseVarCount
            v <- gets variableCount
            emit $ Variable $ Ident $ show v
            emit $ Srem I64 (VIdent (Ident $ show vabs)) v2

        emitDiv :: Exp -> Exp -> CompilerState
        emitDiv e1 e2 = do
            (v1,v2) <- binExprToValues e1 e2
            increaseVarCount
            v <- gets variableCount
            emit $ Variable $ Ident $ show v
            emit $ Div I64 v1 v2

        emitSub :: Exp -> Exp -> CompilerState
        emitSub e1 e2 = do
            (v1,v2) <- binExprToValues e1 e2
            increaseVarCount
            v <- gets variableCount
            emit $ Variable $ Ident $ show v
            emit $ Sub I64 v1 v2

        binExprToValues :: Exp -> Exp -> State CodeGenerator (Value, Value)
        binExprToValues e1 e2 = case (e1, e2) of
                    -- instead of declaring variables for working on ints,
                    -- we can directly pass them to their functions.
                    -- This optimization removes the need to declare
                    -- roughly 50% of variables
                    (EInt i1, EInt i2) -> return (VInteger i1, VInteger i2)
                    (EInt i, e) -> do
                        go e
                        v2 <- gets variableCount
                        return (VInteger i, VIdent (Ident $ show v2))
                    (e, EInt i) -> do
                        go e
                        v2 <- gets variableCount
                        return (VIdent (Ident $ show v2), VInteger i)
                    (e1, e2) -> do
                        go e1
                        v1 <- gets variableCount
                        go e2
                        v2 <- gets variableCount
                        return (VIdent (Ident $ show v1),VIdent (Ident $ show v2))


-- very nasty
flattenFuncType :: Type -> (LLType, [LLType])
flattenFuncType xs = do
    let res = go xs
    (last res, init res)
    where
        go TInt         = [I64]
        go (TPol id)    = [CustomType id]
        go (TFun t1 t2) = go t1 ++ go t2
