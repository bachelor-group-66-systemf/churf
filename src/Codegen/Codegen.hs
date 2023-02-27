{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen.Codegen (compile) where

import           Auxiliary                 (snoc)
import           Codegen.LlvmIr            (LLVMIr (..), LLVMType (..),
                                            LLVMValue (..), Visibility (..),
                                            llvmIrToString)
import           Control.Monad.State       (StateT, execStateT, gets, modify)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Tuple.Extra          (dupe, first, second)
import           Grammar.ErrM              (Err)
import           TypeChecker.TypeChecker
import           TypeChecker.TypeCheckerIr

-- | The record used as the code generator state
data CodeGenerator = CodeGenerator
    { instructions  :: [LLVMIr]
    , functions     :: Map Id FunctionInfo
    , variableCount :: Integer
    }

-- | A state type synonym
type CompilerState a = StateT CodeGenerator Err a

data FunctionInfo = FunctionInfo
    { numArgs   :: Int
    , arguments :: [Id]
    }

-- | Adds a instruction to the CodeGenerator state
emit :: LLVMIr -> CompilerState ()
emit l = modify $ \t -> t { instructions = snoc l $ instructions t }

-- | Increases the variable counter in the CodeGenerator state
increaseVarCount :: CompilerState ()
increaseVarCount = modify $ \t -> t { variableCount = variableCount t + 1 }

-- | Returns the variable count from the CodeGenerator state
getVarCount :: CompilerState Integer
getVarCount = gets variableCount

-- | Increases the variable count and returns it from the CodeGenerator state
getNewVar :: CompilerState Integer
getNewVar = increaseVarCount >> getVarCount

-- | Produces a map of functions infos from a list of binds,
-- which contains useful data for code generation.
getFunctions :: [Bind] -> Map Id FunctionInfo
getFunctions bs = Map.fromList $ map go bs
  where
    go (Bind id args _) =
        (id, FunctionInfo { numArgs=length args, arguments=args })



initCodeGenerator :: [Bind] -> CodeGenerator
initCodeGenerator scs = CodeGenerator { instructions = defaultStart
                                      , functions = getFunctions scs
                                      , variableCount = 0
                                      }

-- | Compiles an AST and produces a LLVM Ir string.
-- An easy way to actually "compile" this output is to
-- Simply pipe it to lli
compile :: Program -> Err String
compile (Program scs) = do
    let codegen = initCodeGenerator scs
    llvmIrToString . instructions <$> execStateT (compileScs scs) codegen

compileScs :: [Bind] -> CompilerState ()
compileScs []                             = pure ()
compileScs (Bind (name, t) args exp : xs) = do
    emit $ UnsafeRaw "\n"
    emit . Comment $ show name <> ": " <> show exp
    let args' = map (second type2LlvmType) args
    emit $ Define (type2LlvmType t_return) name args'
    functionBody <- exprToValue exp
    if name == "main"
        then mapM_ emit $ mainContent functionBody
        else emit $ Ret I64 functionBody
    emit DefineEnd
    modify $ \s -> s { variableCount = 0 }
    compileScs xs
  where
    t_return = snd $ partitionType (length args) t

mainContent :: LLVMValue -> [LLVMIr]
mainContent var =
    [ UnsafeRaw $
        "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef " <> show var <> ")\n"
    , -- , SetVariable (Ident "p") (Icmp LLEq I64 (VInteger 2) (VInteger 2))
      -- , BrCond (VIdent (Ident "p")) (Ident "b_1") (Ident "b_2")
      -- , Label (Ident "b_1")
      -- , UnsafeRaw
      --     "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef 1)\n"
      -- , Br (Ident "end")
      -- , Label (Ident "b_2")
      -- , UnsafeRaw
      --     "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef 2)\n"
      -- , Br (Ident "end")
      -- , Label (Ident "end")
      Ret I64 (VInteger 0)
    ]

defaultStart :: [LLVMIr]
defaultStart = [ UnsafeRaw "@.str = private unnamed_addr constant [3 x i8] c\"%i\n\", align 1\n"
               , UnsafeRaw "declare i32 @printf(ptr noalias nocapture, ...)\n"
               ]

compileExp :: Exp -> CompilerState ()
compileExp = \case
  ELit _ (LInt i)        -> emitInt i
  EAdd t e1 e2  -> emitAdd t e1 e2
  EId (name, _) -> emitIdent name
  EApp t e1 e2  -> emitApp t e1 e2
  EAbs t ti e   -> emitAbs t ti e
  ELet bind e   -> emitLet bind e

--- aux functions ---
emitAbs :: Type -> Id -> Exp -> CompilerState ()
emitAbs _t tid e = emit . Comment $ "Lambda escaped previous stages: \\" <> show tid <> " . " <> show e

emitLet :: Bind -> Exp -> CompilerState ()
emitLet b e = emit . Comment $ concat [ "ELet ("
                                      , show b
                                      , " = "
                                      , show e
                                      , ") is not implemented!"
                                      ]

emitApp :: Type -> Exp -> Exp -> CompilerState ()
emitApp t e1 e2 = appEmitter t e1 e2 []
  where
    appEmitter :: Type -> Exp -> Exp -> [Exp] -> CompilerState ()
    appEmitter t e1 e2 stack = do
        let newStack = e2 : stack
        case e1 of
            EApp _ e1' e2' -> appEmitter t e1' e2' newStack
            EId id@(name, _) -> do
                args <- traverse exprToValue newStack
                vs <- getNewVar
                funcs <- gets functions
                let visibility = maybe Local (const Global) $ Map.lookup id funcs
                    args'      = map (first valueGetType . dupe) args
                    call       = Call (type2LlvmType t) visibility name args'
                emit $ SetVariable (Ident $ show vs) call
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

emitAdd :: Type -> Exp -> Exp -> CompilerState ()
emitAdd t e1 e2 = do
    v1 <- exprToValue e1
    v2 <- exprToValue e2
    v <- getNewVar
    emit $ SetVariable (Ident $ show v) (Add (type2LlvmType t) v1 v2)

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
exprToValue = \case
    ELit _ (LInt i) -> pure $ VInteger i

    EId id@(name, t) -> do
        funcs <- gets functions
        case Map.lookup id funcs of
            Just fi -> do
                if numArgs fi == 0
                    then do
                        vc <- getNewVar
                        emit $ SetVariable (Ident $ show vc)
                            (Call (type2LlvmType t) Global name [])
                        pure $ VIdent (Ident $ show vc) (type2LlvmType t)
                    else pure $ VFunction name Global (type2LlvmType t)
            Nothing -> pure $ VIdent name (type2LlvmType t)

    e -> do
        compileExp e
        v <- getVarCount
        pure $ VIdent (Ident $ show v) (getType e)

type2LlvmType :: Type -> LLVMType
type2LlvmType = \case
    (TMono "Int") -> I64
    TArr t xs -> do
        let (t', xs') = function2LLVMType xs [type2LlvmType t]
        Function t' xs'
    t -> I64 --CustomType $ Ident ("\"" ++ show t ++ "\"")
  where
    function2LLVMType :: Type -> [LLVMType] -> (LLVMType, [LLVMType])
    function2LLVMType (TArr t xs) s = function2LLVMType xs (type2LlvmType t : s)
    function2LLVMType x s           = (type2LlvmType x, s)

getType :: Exp -> LLVMType
getType (ELit _ (LInt _)) = I64
getType (EAdd t _ _)      = type2LlvmType t
getType (EId (_, t))      = type2LlvmType t
getType (EApp t _ _)      = type2LlvmType t
getType (EAbs t _ _)      = type2LlvmType t
getType (ELet _ e)        = getType e

valueGetType :: LLVMValue -> LLVMType
valueGetType (VInteger _)      = I64
valueGetType (VIdent _ t)      = t
valueGetType (VConstant s)     = Array (length s) I8
valueGetType (VFunction _ _ t) = t

-- | Partion type into types of parameters and return type.
partitionType :: Int -- Number of parameters to apply
              -> Type
              -> ([Type], Type)
partitionType = go []
  where
    go acc 0 t = (acc, t)
    go acc i t = case t of
        TArr t1 t2 -> go (snoc t1 acc) (i - 1) t2
        _          -> error "Number of parameters and type doesn't match"
