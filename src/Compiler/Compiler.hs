{-# LANGUAGE LambdaCase #-}
module Compiler.Compiler where

import           Compiler.StandardLLVMLibrary (standardLLVMLibrary)
import           Control.Monad.State          (State, execState, gets, modify)
import           Data.List                    (intercalate)
import           Data.Set                     as Set
import           Grammar.Abs                  (Def (..), Exp (..), Ident (..),
                                               Program (..), Type (..))
import           Grammar.Print                (printTree)

-- | A datatype which represents some basic LLVM types
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

-- | Represents a LLVM "value", as in an integer, a register variable,
--   or a string contstant
data Value = VInteger Integer | VIdent Ident | VConstant String
instance Show Value where
    show :: Value -> String
    show v = case v of
        VInteger i       -> show i
        VIdent (Ident i) -> "%" <> i
        VConstant s      -> "c" <> show s

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

-- | A datatype which represents different instructions in LLVM
data LLVMIr = Define LLType Ident Params
            | DefineEnd
            | Declare LLType Ident Params
            | SetVariable Ident
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
            | Comment String
            | UnsafeRaw String -- This should generally be avoided, and proper
                               -- instructions should be used in its place
    deriving (Show)

-- | Converts a LLVM inststruction to a String, allowing for printing etc.
printLLVMIr :: LLVMIr -> String
printLLVMIr = \case
    (Define t (Ident i) params)           -> concat ["define ", show t, " @", i, "("
                                                    , intercalate "," (fmap (\(x,Ident y) -> unwords [show x, "%"<>y]) params)
                                                    ,") {\n"]
    DefineEnd                             -> "}\n"
    (Declare _t (Ident _i) _params)       -> undefined
    (SetVariable (Ident i))               -> concat ["%", i, " = "]
    (Add t v1 v2)                         -> concat ["add ", show t, " "
                                                    , show v1, ", ", show v2
                                                    , "\n"]
    (Sub t v1 v2)                         -> concat ["sub ", show t, " "
                                                    , show v1, ", "
                                                    , show v2, "\n"]
    (Div t v1 v2)                         -> concat ["sdiv ", show t, " "
                                                    , show v1, ", "
                                                    , show v2, "\n"]
    (Mul t v1 v2)                         -> concat ["mul ", show t, " "
                                                    , show v1, ", "
                                                    , show v2, "\n"]
    (Srem t v1 v2)                        -> concat ["srem ", show t, " "
                                                    , show v1, ", "
                                                    , show v2, "\n"]
    (Call t (Ident i) arg)                -> concat ["call ", show t, " @", i, "("
                                                   , intercalate ", " $ Prelude.map (\(x,y) -> show x <> " " <> show y) arg
                                                   , ")\n"]
    (Alloca t)                            -> unwords ["alloca", show t, "\n"]
    (Store t1 (Ident id1) t2 (Ident id2)) -> concat ["store ", show t1, " %"
                                                    , id1, ", ", show t2, " %"
                                                    , id2, "\n"]
    (Bitcast t1 (Ident i) t2)             -> concat ["bitcast ", show t1, " %"
                                                    , i, " to ", show t2, "\n"]
    (Ret t v)                             -> concat ["ret ", show t
                                                    , " ", show v, "\n"]
    (UnsafeRaw s)                         -> s
    (Comment s)                           -> "; " <> s <> "\n"

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
flattenFuncType :: Type -> (LLType, [LLType])
flattenFuncType xs = do
    let res = go xs
    (last res, init res)
    where
        go TInt         = [I64]
        go (TPol id)    = [CustomType id]
        go (TFun t1 t2) = go t1 ++ go t2
