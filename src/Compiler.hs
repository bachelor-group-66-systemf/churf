{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler (compile) where

import           Control.Monad.State  (StateT, execStateT, gets, modify)
import           Data.List.Extra      (trim)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Tuple.Extra     (second)
import           Grammar.ErrM         (Err)
import           Grammar.Print        (printTree)
import           LlvmIr               (LLVMComp (..), LLVMIr (..),
                                       LLVMType (..), LLVMValue (..),
                                       Visibility (..), llvmIrToString)
import           System.IO            (stdin)
import           System.Process.Extra (CreateProcess (std_in),
                                       StdStream (CreatePipe), createProcess,
                                       readCreateProcess, shell)
import           TypeChecker          (partitionType)
import           TypeCheckerIr

-- | The record used as the code generator state
data CodeGenerator = CodeGenerator
    { instructions  :: [LLVMIr]
    , functions     :: Map Id FunctionInfo
    , variableCount :: Integer
    , labelCount    :: Integer
    }

-- | A state type synonym
type CompilerState a = StateT CodeGenerator Err a

data FunctionInfo = FunctionInfo
    { numArgs   :: Int
    , arguments :: [Id]
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

-- | Increses the label count and returns a label from the CodeGenerator state
getNewLabel :: CompilerState Integer
getNewLabel = do
    modify (\t -> t{labelCount = labelCount t + 1})
    gets labelCount

{- | Produces a map of functions infos from a list of binds,
  which contains useful data for code generation.
-}
getFunctions :: [Bind] -> Map Id FunctionInfo
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

run :: Err String -> IO ()
run s = do
    let s' = case s of
            Right s -> s
            Left _  -> error "yo"
    writeFile "llvm.ll" s'
    putStrLn . trim =<< readCreateProcess (shell "lli") s'
test :: Integer -> Program
test v = Program [
    Bind (Ident "fibonacci", TInt) [(Ident "x", TInt)] (
        ECased (EId ("x", TInt)) [
            Case (CInt 0) (EInt 0),
            Case (CInt 1) (EInt 1),
            Case CatchAll (EAdd TInt
                (EApp TInt (EId (Ident "fibonacci", TInt)) (
                    EAdd TInt (EId (Ident "x", TInt))
                              (EInt (fromIntegral ((maxBound :: Int) * 2)))
                ))
                (EApp TInt (EId (Ident "fibonacci", TInt)) (
                    EAdd TInt (EId (Ident "x", TInt))
                    (EInt (fromIntegral ((maxBound :: Int) * 2 + 1)))
                ))
            )
        ]
    ),
    Bind (Ident "main",TInt) [] (
       EApp TInt (EId (Ident "fibonacci", TInt))  (EInt v) -- (EInt 92)
    )
    ]

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
                , labelCount = 0
                }
    ins <- instructions <$> execStateT (goDef prg) s
    pure $ llvmIrToString ins
  where
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
    defaultStart =
        [ Comment (show $ printTree (Program prg))
        , UnsafeRaw "@.str = private unnamed_addr constant [3 x i8] c\"%i\n\", align 1\n"
        , UnsafeRaw "declare i32 @printf(ptr noalias nocapture, ...)\n"
        ]

    goDef :: [Bind] -> CompilerState ()
    goDef [] = return ()
    goDef (Bind (name, t) args exp : xs) = do
        emit $ UnsafeRaw "\n"
        emit $ Comment $ show name <> ": " <> show exp
        emit $ Define (I64{-type2LlvmType t_return-}) name (map (second type2LlvmType) args)
        functionBody <- exprToValue exp
        if name == "main"
            then mapM_ emit (mainContent functionBody)
            else emit $ Ret I64 functionBody
        emit DefineEnd
        modify (\s -> s{variableCount = 0})
        goDef xs
      where
        t_return = snd $ partitionType (length args) t

    go :: Exp -> CompilerState ()
    go (EInt int)      = emitInt int
    go (EAdd t e1 e2)  = emitAdd t e1 e2
    go (EId (name, _)) = emitIdent name
    go (EApp t e1 e2)  = emitApp t e1 e2
    go (EAbs t ti e)   = emitAbs t ti e
    go (ELet binds e)  = emitLet binds e
    go (EAnn _ _)      = emitEAnn
    go (ECased e c)    = emitECased e c
    -- go (ESub e1 e2)  = emitSub e1 e2
    -- go (EMul e1 e2)  = emitMul e1 e2
    -- go (EDiv e1 e2)  = emitDiv e1 e2
    -- go (EMod e1 e2)  = emitMod e1 e2

    --- aux functions ---
    emitECased :: Exp -> [Case] -> CompilerState ()
    emitECased e cs = do
        vs <- exprToValue e
        lbl <- getNewLabel
        let label = Ident $ "escape_" <> show lbl
        stackPtr <- getNewVar
        emit $ SetVariable (Ident $ show stackPtr) (Alloca I64)
        mapM_ (emitCases label stackPtr vs) cs
        emit $ Label label
        res <- getNewVar
        emit $ SetVariable (Ident $ show res) (Load I64 Ptr (Ident $ show stackPtr))
      where
        emitCases :: Ident -> Integer -> LLVMValue ->  Case -> CompilerState ()
        emitCases label stackPtr vs (Case (CInt i) exp) = do
            ns <- getNewVar
            lbl_fail <- getNewLabel
            lbl_succ <- getNewLabel
            let failed = Ident $ "failed_" <> show lbl_fail
            let success = Ident $ "success_" <> show lbl_succ
            emit $ SetVariable (Ident $ show ns) (Icmp LLEq I64 vs (VInteger i))
            emit $ BrCond (VIdent (Ident $ show ns) I64) success failed
            emit $ Label success
            val <- exprToValue exp
            emit $ Store I64 val Ptr (Ident . show $ stackPtr)
            emit $ Br label
            emit $ Label failed
        emitCases label stackPtr  _ (Case CatchAll exp) = do
            val <- exprToValue exp
            emit $ Store I64 val Ptr (Ident . show $ stackPtr)
            emit $ Br label


    emitEAnn :: CompilerState ()
    emitEAnn = emit . UnsafeRaw $ "Annotated escaped previous stages"

    emitAbs :: Type -> Id -> Exp -> CompilerState ()
    emitAbs _t tid e = do
        emit . Comment $
            "Lambda escaped previous stages: \\" <> show tid <> " . " <> show e
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
                    let vis = case Map.lookup id funcs of
                            Nothing -> Local
                            Just _  -> Global
                    let call = Call (type2LlvmType t) vis name ((\x -> (valueGetType x, x)) <$> args)
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
    exprToValue (EInt i) = return $ VInteger i
    exprToValue (EId id@(name, t)) = do
        funcs <- gets functions
        case Map.lookup id funcs of
            Just fi -> do
                if numArgs fi == 0
                    then do
                        vc <- getNewVar
                        emit $ SetVariable (Ident $ show vc) (Call (type2LlvmType t) Global name [])
                        return $ VIdent (Ident $ show vc) (type2LlvmType t)
                    else return $ VFunction name Global (type2LlvmType t)
            Nothing -> return $ VIdent name (type2LlvmType t)
    exprToValue e = do
        go e
        v <- getVarCount
        return $ VIdent (Ident $ show v) (getType e)

type2LlvmType :: Type -> LLVMType
type2LlvmType = \case
    TInt -> I64
    TFun t xs -> do
        let (t', xs') = function2LLVMType xs [type2LlvmType t]
        Function t' xs'
    t -> CustomType $ Ident ("\"" ++ show t ++ "\"")
  where
    function2LLVMType :: Type -> [LLVMType] -> (LLVMType, [LLVMType])
    function2LLVMType (TFun t xs) s = function2LLVMType xs (type2LlvmType t : s)
    function2LLVMType x s           = (type2LlvmType x, s)

getType :: Exp -> LLVMType
getType (EInt _)     = I64
getType (EAdd t _ _) = type2LlvmType t
getType (EId (_, t)) = type2LlvmType t
getType (EApp t _ _) = type2LlvmType t
getType (EAbs t _ _) = type2LlvmType t
getType (ELet _ e)   = getType e
getType (EAnn _ t)   = type2LlvmType t

valueGetType :: LLVMValue -> LLVMType
valueGetType (VInteger _)      = I64
valueGetType (VIdent _ t)      = t
valueGetType (VConstant s)     = Array (length s) I8
valueGetType (VFunction _ _ t) = t
