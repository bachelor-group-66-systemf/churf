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

data Type = I8 | I32 | I64 | Ptr | Ref Type | Array Integer Type
instance Show Type where
    show :: Type -> String
    show t = case t of
        I8         -> "i8"
        I32        -> "i32"
        I64        -> "i64"
        Ptr        -> "ptr"
        Ref ty     -> show ty <> "*"
        Array n ty -> concat ["[", show n, " x ", show ty, "]"]

type Params = [Type]
type Args = [(Type, Value)]

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
defaultCodeGenerator = CodeGenerator {instructions=[], methods=[], blocks=[], variableCount=0}

data LLVMIr = Define Type Ident Params
            | DefineEnd
            | Declare Type Ident Params
            | Variable Ident
            | Add Type Value Value
            | Call Type Ident Args
            | Alloca Type
            | Store Type Ident Type Ident
            | Bitcast Type Ident Type
            | Ret Type Value
            | UnsafeRaw String
            | Comment String
    deriving (Show)

printLLVMIr :: LLVMIr -> String
printLLVMIr (Define t (Ident i) params)           = concat ["define ", show t, " @", i, "(", intercalate "," (fmap show params),") {\n"]
printLLVMIr DefineEnd                             = "}\n"
printLLVMIr (Declare t (Ident i) params)          = undefined
printLLVMIr (Variable (Ident i))                  = concat ["%", i, " = "]
printLLVMIr (Add t v1 v2)                         = concat ["add ", show t, " ", show v1, ", ", show v2, "\n"]
printLLVMIr (Call t (Ident i) arg)                = concat ["call ", show t, " @", i, "("
                                                           , concatMap (\(x,y) -> show x <> " " <> show y) arg
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
compile (Program e) = do
    --Asp
    let s = defaultCodeGenerator {instructions = [
        Comment $ printTree (Program e),
        UnsafeRaw $ standardLLVMLibrary <> "\n",
        --UnsafeRaw "declare i32 @puts(i8* nocapture) nounwind\n",
        --UnsafeRaw "declare [21 x i8] @i64ToString(i64)\n",
        Define I32 (Ident "main") []
    ]}
    let fin = execState (go e) s
    let ins = instructions fin
    let var = variableCount fin
    putStrLn $ concatMap printLLVMIr (ins ++
        [ Variable (Ident "print_res")
        , Call (Array 21 I8) (Ident "i64ToString") [(I64, VIdent $ Ident $ show var)]
        , Variable (Ident "print_ptr"), Alloca (Array 21 I8)
        , Store (Array 21 I8) (Ident "print_res") (Ref (Array 21 I8)) (Ident "print_ptr")
        , Variable (Ident "printable"), Bitcast (Ref (Array 21 I8)) (Ident "print_ptr") (Ref I8)
        , Call I32 (Ident "puts") [(Ref I8, VIdent (Ident "printable"))]
        , Ret I32 (VInteger 0)
        , DefineEnd
        ])
    where
        go :: Exp -> CompilerState
        go (EId  id)    = undefined
        go (EInt int)   = do
            increaseVarCount
            varCount <- gets variableCount
            emit $ Variable $ Ident (show varCount)
            emit $ Add I64 (VInteger int) (VInteger 0)

        go (EApp e1 e2) = undefined
        go (EAdd e1 e2) = emitAdd e1 e2
        go (EAbs id e)  = undefined

        --- aux functions ---
        emitAdd :: Exp -> Exp -> CompilerState
        emitAdd (EInt i1) (EInt i2) = do
            -- instead of declaring two variables for this case,
            -- we can directly pass them to add.
            -- //TODO This should also be done with (EInt & Exp) and (Exp &EInt)
            increaseVarCount
            v1 <- gets variableCount
            emit $ Variable $ Ident $ show v1
            emit $ Add I64 (VInteger i1) (VInteger i2)
        emitAdd e1 e2 = do
            go e1
            v1 <- gets variableCount
            go e2
            v2 <- gets variableCount
            increaseVarCount
            v3 <- gets variableCount
            emit $ Variable $ Ident $ show v3
            emit $ Add I64 (VIdent (Ident $ show v1)) (VIdent (Ident $ show v2))

