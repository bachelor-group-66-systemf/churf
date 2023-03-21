{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen.Codegen (generateCode) where
import           Auxiliary                     (snoc)
import           Codegen.LlvmIr                (CallingConvention (..),
                                                LLVMComp (..), LLVMIr (..),
                                                LLVMType (..), LLVMValue (..),
                                                Visibility (..), llvmIrToString)
import           Codegen.LlvmIr                as LIR
import           Control.Applicative           ((<|>))
import           Control.Monad.State           (StateT, execStateT, foldM_,
                                                gets, modify)
import qualified Data.Bifunctor                as BI
import           Data.List.Extra               (trim)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Tuple.Extra              (dupe, first, second)
import qualified Grammar.Abs                   as GA
import           Grammar.ErrM                  (Err)
import           Monomorphizer.MonomorphizerIr as MIR
import           System.Process.Extra          (readCreateProcess, shell)
-- | The record used as the code generator state
data CodeGenerator = CodeGenerator
    { instructions  :: [LLVMIr]
    , functions     :: Map Id FunctionInfo
    , constructors  :: Map Id ConstructorInfo
    , variableCount :: Integer
    , labelCount    :: Integer
    }

-- | A state type synonym
type CompilerState a = StateT CodeGenerator Err a

data FunctionInfo = FunctionInfo
    { numArgs   :: Int
    , arguments :: [Id]
    }
data ConstructorInfo = ConstructorInfo
    { numArgsCI   :: Int
    , argumentsCI :: [Id]
    , numCI       :: Integer
    }


-- | Adds a instruction to the CodeGenerator state
emit :: LLVMIr -> CompilerState ()
emit l = modify $ \t -> t { instructions = Auxiliary.snoc l $ instructions t }

-- | Increases the variable counter in the CodeGenerator state
increaseVarCount :: CompilerState ()
increaseVarCount = modify $ \t -> t { variableCount = variableCount t + 1 }

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

-- | Produces a map of functions infos from a list of binds,
--  which contains useful data for code generation.
getFunctions :: [Bind] -> Map Id FunctionInfo
getFunctions bs = Map.fromList $ go bs
  where
    go [] = []
    go (Bind id args _ : xs) =
        (id, FunctionInfo { numArgs=length args, arguments=args })
        : go xs
    go (DataType n cons : xs) = do
        map (\(Constructor id xs) -> ((id, MIR.Type n), FunctionInfo {
            numArgs=length xs, arguments=createArgs xs
        })) cons
        <> go xs

createArgs :: [Type] -> [Id]
createArgs xs = fst $ foldl (\(acc, l) t -> (acc ++ [(GA.Ident ("arg_" <> show l) , t)],l+1)) ([], 0) xs

-- | Produces a map of functions infos from a list of binds,
--  which contains useful data for code generation.
getConstructors :: [Bind] -> Map Id ConstructorInfo
getConstructors bs = Map.fromList $ go bs
  where
    go [] = []
    go (DataType (GA.Ident n) cons : xs) = do
        fst (foldl (\(acc,i) (Constructor (GA.Ident id) xs) -> (((GA.Ident (n <> "_" <> id), MIR.Type (GA.Ident n)), ConstructorInfo {
            numArgsCI=length xs,
            argumentsCI=createArgs xs,
            numCI=i
        }) : acc, i+1)) ([],0) cons)
        <> go xs
    go (_: xs) = go xs

initCodeGenerator :: [Bind] -> CodeGenerator
initCodeGenerator scs = CodeGenerator { instructions = defaultStart
                                      , functions = getFunctions scs
                                      , constructors = getConstructors scs
                                      , variableCount = 0
                                      , labelCount = 0
                                      }

run :: Err String -> IO ()
run s = do
    let s' = case s of
            Right s -> s
            Left _  -> error "yo"
    writeFile "output/llvm.ll" s'
    putStrLn . trim =<< readCreateProcess (shell "lli") s'

test :: Integer -> Program
test v = Program [
    DataType (GA.Ident "Craig") [
        Constructor (GA.Ident "Bob") [MIR.Type (GA.Ident "_Int")]--,
        --(GA.Ident "Alice", [TInt, TInt])
    ],
    Bind (GA.Ident "fibonacci", MIR.Type (GA.Ident "_Int")) [(GA.Ident "x", MIR.Type (GA.Ident "_Int"))] (EId ("x", MIR.Type (GA.Ident "Craig")), MIR.Type (GA.Ident "Craig")),
    Bind (GA.Ident "main", MIR.Type (GA.Ident "_Int")) []
       (EApp (MIR.Type (GA.Ident "Craig")) (EId (GA.Ident "Craig_Bob", MIR.Type (GA.Ident "Craig")), MIR.Type (GA.Ident "Craig")) (ELit (LInt v), MIR.Type (GA.Ident "_Int")), MIR.Type (GA.Ident "Craig"))-- (EInt 92)
    ]

{- | Compiles an AST and produces a LLVM Ir string.
  An easy way to actually "compile" this output is to
  Simply pipe it to LLI
-}
generateCode :: Program -> Err String
generateCode (Program scs) = do
    let codegen = initCodeGenerator scs
    llvmIrToString . instructions <$> execStateT (compileScs scs) codegen

compileScs :: [Bind] -> CompilerState ()
compileScs []                             = do
    -- as a last step create all the constructors
    -- //TODO maybe merge this with the data type match?
    c <- gets (Map.toList . constructors)
    mapM_ (\((id, t), ci) -> do
            let t' = type2LlvmType t
            let x = BI.second type2LlvmType <$> argumentsCI ci
            emit $ Define FastCC t' id x
            top <- GA.Ident . show <$> getNewVar
            ptr <- GA.Ident . show <$> getNewVar
            -- allocated the primary type
            emit $ SetVariable top (Alloca t')

             -- set the first byte to the index of the constructor
            emit $ SetVariable ptr $
                GetElementPtrInbounds t' (Ref t')
                                      (VIdent top I8) I32 (VInteger 0) I32 (VInteger 0)
            emit $ Store I8 (VInteger $ numCI ci ) (Ref I8) ptr

             -- get a pointer of the correct type
            ptr' <- GA.Ident . show <$> getNewVar
            emit $ SetVariable ptr' (Bitcast (Ref t') ptr (Ref $ CustomType id))

             --emit $ UnsafeRaw "\n"

            foldM_ (\i (GA.Ident arg_n, arg_t)-> do
                let arg_t' = type2LlvmType arg_t
                emit $ Comment (show arg_t' <>" "<> arg_n <> " " <> show i )
                elemPtr <- GA.Ident . show <$> getNewVar
                emit $ SetVariable elemPtr (
                    GetElementPtrInbounds (CustomType id) (Ref (CustomType id))
                                          (VIdent ptr' Ptr) I32
                                          (VInteger 0) I32 (VInteger i))
                emit $ Store arg_t' (VIdent (GA.Ident arg_n) arg_t') Ptr elemPtr
                -- %2 = getelementptr inbounds %Foo_AInteger, %Foo_AInteger* %1, i32 0, i32 1
                -- store i32 42, i32* %2
                pure $ i + 1-- + typeByteSize arg_t'
                ) 1 (argumentsCI ci)

            --emit $ UnsafeRaw "\n"

            -- load and return the constructed value
            load <- GA.Ident . show <$> getNewVar
            emit $ SetVariable load (Load t' Ptr top)
            emit $ Ret t' (VIdent load t')
            emit DefineEnd

            modify $ \s -> s { variableCount = 0 }
        ) c
compileScs (Bind (name, _t) args exp : xs) = do
    emit $ UnsafeRaw "\n"
    emit . Comment $ show name <> ": " <> show exp
    let args' = map (second type2LlvmType) args
    emit $ Define FastCC I64 {-(type2LlvmType t_return)-} name args'
    functionBody <- exprToValue (fst exp)
    if name == "main"
        then mapM_ emit $ mainContent functionBody
        else emit $ Ret I64 functionBody
    emit DefineEnd
    modify $ \s -> s { variableCount = 0 }
    compileScs xs
compileScs (DataType id@(GA.Ident outer_id) ts : xs) = do
    let biggest_variant = maximum ((\(Constructor _ t) -> sum $ typeByteSize . type2LlvmType <$> t) <$> ts)
    emit $ LIR.Type id [I8, Array biggest_variant I8]
    mapM_ (\(Constructor (GA.Ident inner_id) fi) -> do
            emit $ LIR.Type (GA.Ident $ outer_id <> "_" <> inner_id) (I8 : map type2LlvmType fi)
        ) ts
    compileScs xs

   -- where
   --   _t_return = snd $ partitionType (length args) t

mainContent :: LLVMValue -> [LLVMIr]
mainContent var =
    [ UnsafeRaw $
        "%2 = alloca %Craig\n" <>
        "    store %Craig %1, ptr %2\n" <>
        "    %3 = bitcast %Craig* %2 to i64*\n" <>
        "    %4 = load i64, ptr %3\n" <>
        "    call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef %4)\n"
      --    "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef " <> show var <> ")\n"
    , -- , SetVariable (GA.Ident "p") (Icmp LLEq I64 (VInteger 2) (VInteger 2))
      -- , BrCond (VIdent (GA.Ident "p")) (GA.Ident "b_1") (GA.Ident "b_2")
      -- , Label (GA.Ident "b_1")
      -- , UnsafeRaw
      --     "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef 1)\n"
      -- , Br (GA.Ident "end")
      -- , Label (GA.Ident "b_2")
      -- , UnsafeRaw
      --     "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef 2)\n"
      -- , Br (GA.Ident "end")
      -- , Label (GA.Ident "end")
      Ret I64 (VInteger 0)
    ]

defaultStart :: [LLVMIr]
defaultStart = [ UnsafeRaw "target triple = \"x86_64-pc-linux-gnu\"\n"
               , UnsafeRaw "target datalayout = \"e-m:o-i64:64-f80:128-n8:16:32:64-S128\"\n"
               , UnsafeRaw "@.str = private unnamed_addr constant [3 x i8] c\"%i\n\", align 1\n"
               , UnsafeRaw "declare i32 @printf(ptr noalias nocapture, ...)\n"
               ]

compileExp :: Exp -> CompilerState ()
compileExp (ELit lit)       = emitLit lit
compileExp (EAdd t e1 e2)   = emitAdd t (fst e1) (fst e2)
--compileExp (ESub t e1 e2)  = emitSub t e1 e2
compileExp (EId (name, _))  = emitIdent name
compileExp (EApp t e1 e2)   = emitApp t (fst e1) (fst e2)
--compileExp (EAbs t ti e)   = emitAbs t ti e
compileExp (ELet _ binds e) = undefined emitLet binds (fst e)
compileExp (ECase t e cs)   = emitECased t (fst e) (map (t,) cs)
    -- go (EMul e1 e2)  = emitMul e1 e2
    -- go (EDiv e1 e2)  = emitDiv e1 e2
    -- go (EMod e1 e2)  = emitMod e1 e2

--- aux functions ---
emitECased :: Type -> Exp -> [(Type, Injection)] -> CompilerState ()
emitECased t e cases = do
    let cs = snd <$> cases
    let ty = type2LlvmType t
    vs <- exprToValue e
    lbl <- getNewLabel
    let label = GA.Ident $ "escape_" <> show lbl
    stackPtr <- getNewVar
    emit $ SetVariable (GA.Ident $ show stackPtr) (Alloca ty)
    mapM_ (emitCases ty label stackPtr vs) cs
    emit $ Label label
    res <- getNewVar
    emit $ SetVariable (GA.Ident $ show res) (Load ty Ptr (GA.Ident $ show stackPtr))
    where
    emitCases :: LLVMType -> GA.Ident -> Integer -> LLVMValue -> Injection -> CompilerState ()
    emitCases ty label stackPtr vs (Injection (MIR.CLit i) exp) = do
        let i' = case i of
                    LInt i  -> VInteger i
                    LChar i -> VChar i
        ns <- getNewVar
        lbl_failPos <- (\x -> GA.Ident $ "failed_" <> show x) <$> getNewLabel
        lbl_succPos <- (\x -> GA.Ident $ "success_" <> show x) <$> getNewLabel
        emit $ SetVariable (GA.Ident $ show ns) (Icmp LLEq ty vs i')
        emit $ BrCond (VIdent (GA.Ident $ show ns) ty) lbl_succPos lbl_failPos
        emit $ Label lbl_succPos
        val <- exprToValue (fst exp)
        emit $ Store ty val Ptr (GA.Ident . show $ stackPtr)
        emit $ Br label
        emit $ Label lbl_failPos
    emitCases ty label stackPtr  _ (Injection MIR.CatchAll exp) = do
        val <- exprToValue (fst exp)
        emit $ Store ty val Ptr (GA.Ident . show $ stackPtr)
        emit $ Br label


emitLet :: Bind -> Exp -> CompilerState ()
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
            EApp _ (e1', _) (e2', _) -> appEmitter t e1' e2' newStack
            EId id@(GA.Ident name,_ ) -> do
                args <- traverse exprToValue newStack
                vs <- getNewVar
                funcs <- gets functions
                consts <- gets constructors
                let visibility = maybe Local (const Global) $
                        const Global <$ Map.lookup id consts
                        <|>
                        const Global <$ Map.lookup id funcs
                        -- this piece of code could probably be improved, i.e remove the double `const Global`
                    args'      = map (first valueGetType . dupe) args
                    call       = Call FastCC (type2LlvmType t) visibility (GA.Ident name) args'
                emit $ SetVariable (GA.Ident $ show vs) call
            x -> error $ "The unspeakable happened: " <> show x

emitIdent :: GA.Ident -> CompilerState ()
emitIdent id = do
    -- !!this should never happen!!
    emit $ Comment "This should not have happened!"
    emit $ Variable id
    emit $ UnsafeRaw "\n"

emitLit :: Lit -> CompilerState ()
emitLit i = do
    -- !!this should never happen!!
    let (i',t) = case i of
                (LInt i'')  -> (VInteger i'',I64)
                (LChar i'') -> (VChar i'', I8)
    varCount <- getNewVar
    emit $ Comment "This should not have happened!"
    emit $ SetVariable (GA.Ident (show varCount)) (Add t i' (VInteger 0))


emitAdd :: Type -> Exp -> Exp -> CompilerState ()
emitAdd t e1 e2 = do
    v1 <- exprToValue e1
    v2 <- exprToValue e2
    v <- getNewVar
    emit $ SetVariable (GA.Ident $ show v) (Add (type2LlvmType t) v1 v2)

emitSub :: Type -> Exp -> Exp -> CompilerState ()
emitSub t e1 e2 = do
    v1 <- exprToValue e1
    v2 <- exprToValue e2
    v <- getNewVar
    emit $ SetVariable (GA.Ident $ show v) (Sub (type2LlvmType t) v1 v2)

exprToValue :: Exp -> CompilerState LLVMValue
exprToValue = \case
    ELit i -> pure $ case i of
                        (LInt i)  -> VInteger i
                        (LChar i) -> VChar i
    EId id@(name, t) -> do
        funcs <- gets functions
        case Map.lookup id funcs of
            Just fi -> do
                if numArgs fi == 0
                    then do
                        vc <- getNewVar
                        emit $ SetVariable (GA.Ident $ show vc)
                            (Call FastCC (type2LlvmType t) Global name [])
                        pure $ VIdent (GA.Ident $ show vc) (type2LlvmType t)
                    else pure $ VFunction name Global (type2LlvmType t)
            Nothing -> pure $ VIdent name (type2LlvmType t)
    e -> do
        compileExp e
        v <- getVarCount
        pure $ VIdent (GA.Ident $ show v) (getType e)

type2LlvmType :: Type -> LLVMType
type2LlvmType (MIR.Type (GA.Ident t)) = case t of
    "_Int" -> I64
    t      -> CustomType (GA.Ident t)
    -- TInt -> I64
    -- TFun t xs -> do
    --     let (t', xs') = function2LLVMType xs [type2LlvmType t]
    --     Function t' xs'
    -- TPol t -> CustomType t
  --where
  --  function2LLVMType :: Type -> [LLVMType] -> (LLVMType, [LLVMType])
  --  function2LLVMType (TFun t xs) s = function2LLVMType xs (type2LlvmType t : s)
  --  function2LLVMType x s           = (type2LlvmType x, s)

getType :: Exp -> LLVMType
getType (ELit l)          = I64
getType (EAdd t _ _)      = type2LlvmType t
--getType (ESub t _ _)  = type2LlvmType t
getType (EId (_, t))      = type2LlvmType t
getType (EApp t _ _)      = type2LlvmType t
--getType (EAbs t _ _)  = type2LlvmType t
getType (ELet (_, t) _ e) = type2LlvmType t
getType (ECase t _ _)     = type2LlvmType t

valueGetType :: LLVMValue -> LLVMType
valueGetType (VInteger _)      = I64
valueGetType (VIdent _ t)      = t
valueGetType (VConstant s)     = Array (fromIntegral $  length s) I8
valueGetType (VFunction _ _ t) = t

typeByteSize :: LLVMType -> Integer
typeByteSize I1             = 1
typeByteSize I8             = 1
typeByteSize I32            = 4
typeByteSize I64            = 8
typeByteSize Ptr            = 8
typeByteSize (Ref _)        = 8
typeByteSize (Function _ _) = 8
typeByteSize (Array n t)    = n * typeByteSize t
typeByteSize (CustomType _) = 8
