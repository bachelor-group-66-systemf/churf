{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen.Codegen (generateCode) where

import           Auxiliary                     (snoc)
import           Codegen.LlvmIr                as LIR
import           Control.Applicative           ((<|>))
import           Control.Monad.State           (StateT, execStateT, foldM_,
                                                gets, modify)
import qualified Data.Bifunctor                as BI
import           Data.Coerce                   (coerce)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust, fromMaybe)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Tuple.Extra              (dupe, first, second)
import           Grammar.ErrM                  (Err)
import           Monomorphizer.MonomorphizerIr as MIR
import qualified TypeChecker.TypeCheckerIr     as TIR

-- | The record used as the code generator state
data CodeGenerator = CodeGenerator
    { instructions  :: [LLVMIr]
    , functions     :: Map MIR.Id FunctionInfo
    , customTypes   :: Set LLVMType
    , constructors  :: Map TIR.Ident ConstructorInfo
    , variableCount :: Integer
    , labelCount    :: Integer
    }

-- | A state type synonym
type CompilerState a = StateT CodeGenerator Err a

data FunctionInfo = FunctionInfo
    { numArgs   :: Int
    , arguments :: [Id]
    }
    deriving (Show)
data ConstructorInfo = ConstructorInfo
    { numArgsCI    :: Int
    , argumentsCI  :: [Id]
    , numCI        :: Integer
    , returnTypeCI :: MIR.Type
    }
    deriving (Show)

-- | Adds a instruction to the CodeGenerator state
emit :: LLVMIr -> CompilerState ()
emit l = modify $ \t -> t{instructions = Auxiliary.snoc l $ instructions t}

-- | Increases the variable counter in the CodeGenerator state
increaseVarCount :: CompilerState ()
increaseVarCount = do
    gets variableCount >>= \s -> emit.Comment $ "increase: " <> show (s + 1)
    modify $ \t -> t{variableCount = variableCount t + 1}

-- | Returns the variable count from the CodeGenerator state
getVarCount :: CompilerState Integer
getVarCount = gets variableCount

-- | Increases the variable count and returns it from the CodeGenerator state
getNewVar :: CompilerState TIR.Ident
getNewVar = TIR.Ident . show <$> (increaseVarCount >> getVarCount)

-- | Increses the label count and returns a label from the CodeGenerator state
getNewLabel :: CompilerState Integer
getNewLabel = do
    modify (\t -> t{labelCount = labelCount t + 1})
    gets labelCount

{- | Produces a map of functions infos from a list of binds,
 which contains useful data for code generation.
-}
getFunctions :: [MIR.Def] -> Map Id FunctionInfo
getFunctions bs = Map.fromList $ go bs
  where
    go [] = []
    go (MIR.DBind (MIR.Bind id args _) : xs) =
        (id, FunctionInfo{numArgs = length args, arguments = args})
            : go xs
    go (_ : xs) = go xs

createArgs :: [MIR.Type] -> [Id]
createArgs xs = fst $ foldl (\(acc, l) t -> (acc ++ [(TIR.Ident ("arg_" <> show l), t)], l + 1)) ([], 0) xs

{- | Produces a map of functions infos from a list of binds,
 which contains useful data for code generation.
-}
getConstructors :: [MIR.Def] -> Map TIR.Ident ConstructorInfo
getConstructors bs = Map.fromList $ go bs
  where
    go [] = []
    go (MIR.DData (MIR.Data t cons) : xs) = fst
                (foldl (\(acc, i) (Inj id xs) ->
                        (( id, ConstructorInfo
                                { numArgsCI = length (init . flattenType $ xs)
                                , argumentsCI = createArgs (init . flattenType $ xs)
                                , numCI = i
                                , returnTypeCI = t --last . flattenType $ xs
                                }
                          ) : acc, i + 1)) ([], 0) cons) <> go xs
    go (_ : xs) = go xs

getTypes :: [MIR.Def] -> Set LLVMType
getTypes bs = Set.fromList $ go bs
  where
    go []                              = []
    go (MIR.DData (MIR.Data t _) : xs) = type2LlvmType t : go xs
    go (_:xs)                          = go xs

initCodeGenerator :: [MIR.Def] -> CodeGenerator
initCodeGenerator scs =
    CodeGenerator
        { instructions = defaultStart
        , functions = getFunctions scs
        , constructors = getConstructors scs
        , customTypes = getTypes scs
        , variableCount = 0
        , labelCount = 0
        }

{-
run :: Err String -> IO ()
run s = do
    let s' = case s of
            Right s -> s
            Left _  -> error "yo"
    writeFile "output/llvm.ll" s'
    putStrLn . trim =<< readCreateProcess (shell "lli") s'

test :: Integer -> Program
test v =
    Program
        [ DataType
            (TIR.Ident "Craig")
            [ Constructor (TIR.Ident "Bob") [MIR.Type (TIR.Ident "_Int")]
            , Constructor (TIR.Ident "Betty") [MIR.Type (TIR.Ident "_Int")]
            ]
        , DataType
            (TIR.Ident "Alice")
            [ Constructor (TIR.Ident "Eve") [MIR.Type (TIR.Ident "_Int")] -- ,
            -- (TIR.Ident "Alice", [TInt, TInt])
            ]
        , Bind (TIR.Ident "fibonacci", MIR.Type (TIR.Ident "_Int")) [(TIR.Ident "x", MIR.Type (TIR.Ident "_Int"))] (EId ("x", MIR.Type (TIR.Ident "Craig")), MIR.Type (TIR.Ident "Craig"))
        , Bind (TIR.Ident "main", MIR.Type (TIR.Ident "_Int")) []
          -- (EApp (MIR.Type (TIR.Ident "Craig")) (EId (TIR.Ident "Craig_Bob", MIR.Type (TIR.Ident "Craig")), MIR.Type (TIR.Ident "Craig")) (ELit (LInt v), MIR.Type (TIR.Ident "_Int")), MIR.Type (TIR.Ident "Craig"))-- (EInt 92)
          $
            eCaseInt
                (EApp (MIR.TLit (TIR.Ident "Craig")) (EId (TIR.Ident "Craig_Bob", MIR.TLit (TIR.Ident "Craig")), MIR.TLit (TIR.Ident "Craig")) (ELit (LInt v), MIR.Type (TIR.Ident "_Int")), MIR.Type (TIR.Ident "Craig"))
                [ injectionCons "Craig_Bob" "Craig" [CIdent (TIR.Ident "x")] (EId (TIR.Ident "x", MIR.Type (TIR.Ident "_Int")), MIR.Type (TIR.Ident "_Int"))
                , injectionCons "Craig_Betty" "Craig" [CLit (LInt 5)] (int 2)
                , Injection (CIdent (TIR.Ident "z")) (int 3)
                , -- , injectionInt 5 (int 6)
                  injectionCatchAll (int 10)
                ]
        ]
  where
    injectionCons x y xs = Injection (CCons (TIR.Ident x, MIR.Type (TIR.Ident y)) xs)
    injectionInt x = Injection (CLit (LInt x))
    injectionCatchAll = Injection CatchAll
    eCaseInt x xs = (ECase (MIR.TLit (MIR.Ident "_Int")) x xs, MIR.TLit (MIR.Ident "_Int"))
    int x = (ELit (LInt x), MIR.TLit (MIR.Ident "_Int"))
-}
{- | Compiles an AST and produces a LLVM Ir string.
  An easy way to actually "compile" this output is to
  Simply pipe it to LLI
-}
generateCode :: MIR.Program -> Err String
generateCode (MIR.Program scs) = do
    let codegen = initCodeGenerator scs
    llvmIrToString . instructions <$> execStateT (compileScs scs) codegen

compileScs :: [MIR.Def] -> CompilerState ()
compileScs [] = do
    emit $ UnsafeRaw "\n"
    -- as a last step create all the constructors
    -- //TODO maybe merge this with the data type match?
    c <- gets (Map.toList . constructors)
    mapM_
        ( \(id, ci) -> do
            let t = returnTypeCI ci
            let t' = type2LlvmType t
            let x = BI.second type2LlvmType <$> argumentsCI ci
            emit $ Define FastCC t' id x
            top <- getNewVar
            ptr <- getNewVar
            -- allocated the primary type
            emit $ SetVariable top (Alloca t')

            -- set the first byte to the index of the constructor
            emit $
                SetVariable ptr $
                    GetElementPtr
                        t'
                        (Ref t')
                        (VIdent top I8)
                        I64
                        (VInteger 0)
                        I32
                        (VInteger 0)
            emit $ Store I8 (VInteger $ numCI ci) (Ref I8) ptr

            -- get a pointer of the correct type
            ptr' <- getNewVar
            emit $ SetVariable ptr' (Bitcast (Ref t') (VIdent top Ptr) (Ref $ CustomType id))

            enumerateOneM_
                ( \i (TIR.Ident arg_n, arg_t) -> do
                    let arg_t' = type2LlvmType arg_t
                    emit $ Comment (toIr arg_t' <> " " <> arg_n <> " " <> show i)
                    elemPtr <- getNewVar
                    emit $
                        SetVariable
                            elemPtr
                            ( GetElementPtr
                                (CustomType id)
                                (Ref (CustomType id))
                                (VIdent ptr' Ptr)
                                I64
                                (VInteger 0)
                                I32
                                (VInteger i)
                            )
                    emit $ Store arg_t' (VIdent (TIR.Ident arg_n) arg_t') Ptr elemPtr
                )
                (argumentsCI ci)

            -- load and return the constructed value
            emit $ Comment "Return the newly constructed value"
            load <- getNewVar
            emit $ SetVariable load (Load t' Ptr top)
            emit $ Ret t' (VIdent load t')
            emit DefineEnd
            emit $ UnsafeRaw "\n"

            modify $ \s -> s{variableCount = 0}
        )
        c
compileScs (MIR.DBind (MIR.Bind (name, _t) args exp) : xs) = do
    emit $ UnsafeRaw "\n"
    emit . Comment $ show name <> ": " <> show exp
    let args' = map (second type2LlvmType) args
    emit $ Define FastCC I64 {-(type2LlvmType t_return)-} name args'
    functionBody <- exprToValue exp
    if name == "main"
        then mapM_ emit $ mainContent functionBody
        else emit $ Ret I64 functionBody
    emit DefineEnd
    modify $ \s -> s{variableCount = 0}
    compileScs xs
compileScs (MIR.DData (MIR.Data typ ts) : xs) = do
    let (TIR.Ident outer_id) = extractTypeName typ
    let variantTypes fi = init $ map type2LlvmType (flattenType fi)
    let biggestVariant = 7 + maximum (sum . (\(Inj _ fi) -> typeByteSize <$> variantTypes fi) <$> ts)
    emit $ LIR.Type (TIR.Ident outer_id) [I8, Array biggestVariant I8]
    mapM_
        ( \(Inj inner_id fi) -> do
            emit $ LIR.Type inner_id (I8 : variantTypes fi)
        )
        ts
    compileScs xs

mainContent :: LLVMValue -> [LLVMIr]
mainContent var =
    [ UnsafeRaw $
        -- "%2 = alloca %Craig\n" <>
        -- "    store %Craig %1, ptr %2\n" <>
        -- "    %3 = bitcast %Craig* %2 to i72*\n" <>
        -- "    %4 = load i72, ptr %3\n" <>
        -- "    call i32 (ptr, ...) @printf(ptr noundef @.str, i72 noundef %4)\n"

        -- "%2 = alloca %Craig\n" <>
        -- "    store %Craig %1, ptr %2\n" <>
        -- "    %3 = bitcast %Craig* %2 to i72*\n" <>
        -- "    %4 = load i72, ptr %3\n" <>
        -- "    call i32 (ptr, ...) @printf(ptr noundef @.str, i72 noundef %4)\n"

        -- "%2 = alloca %Craig\n" <>
        -- "    store %Craig %1, ptr %2\n" <>
        -- "    %3 = bitcast %Craig* %2 to i72*\n" <>
        -- "    %4 = load i72, ptr %3\n" <>
        -- "    call i32 (ptr, ...) @printf(ptr noundef @.str, i72 noundef %4)\n"
        "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef " <> toIr var <> ")\n"
    , -- , SetVariable (TIR.Ident "p") (Icmp LLEq I64 (VInteger 2) (VInteger 2))
      -- , BrCond (VIdent (TIR.Ident "p")) (TIR.Ident "b_1") (TIR.Ident "b_2")
      -- , Label (TIR.Ident "b_1")
      -- , UnsafeRaw
      --     "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef 1)\n"
      -- , Br (TIR.Ident "end")
      -- , Label (TIR.Ident "b_2")
      -- , UnsafeRaw
      --     "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef 2)\n"
      -- , Br (TIR.Ident "end")
      -- , Label (TIR.Ident "end")
      Ret I64 (VInteger 0)
    ]

defaultStart :: [LLVMIr]
defaultStart =
    [ UnsafeRaw "target triple = \"x86_64-pc-linux-gnu\"\n"
    , UnsafeRaw "target datalayout = \"e-m:o-i64:64-f80:128-n8:16:32:64-S128\"\n"
    , UnsafeRaw "@.str = private unnamed_addr constant [3 x i8] c\"%i\n\", align 1\n"
    , UnsafeRaw "@.non_exhaustive_patterns = private unnamed_addr constant [41 x i8] c\"Non-exhaustive patterns in case at %i:%i\n\"\n"
    , UnsafeRaw "declare i32 @printf(ptr noalias nocapture, ...)\n"
    , UnsafeRaw "declare i32 @exit(i32 noundef)\n"
    ]

compileExp :: ExpT -> CompilerState ()
compileExp (MIR.ELit lit,t)     = emitLit lit
compileExp (MIR.EAdd e1 e2,t)   = emitAdd t e1 e2
-- compileExp (ESub t e1 e2)  = emitSub t e1 e2
compileExp (MIR.EVar name, t)   = emitIdent name
compileExp (MIR.EApp e1 e2,t)   = emitApp t e1 e2
-- compileExp (EAbs t ti e)   = emitAbs t ti e
compileExp (MIR.ELet binds e,t) = undefined -- emitLet binds (fst e)
compileExp (MIR.ECase e cs,t)   = emitECased t e (map (t,) cs)

-- go (EMul e1 e2)  = emitMul e1 e2
-- go (EDiv e1 e2)  = emitDiv e1 e2
-- go (EMod e1 e2)  = emitMod e1 e2

--- aux functions ---
emitECased :: MIR.Type -> ExpT -> [(MIR.Type, Branch)] -> CompilerState ()
emitECased t e cases = do
    let cs = snd <$> cases
    let ty = type2LlvmType t
    let rt = type2LlvmType (snd e)
    vs <- exprToValue e
    lbl <- getNewLabel
    let label = TIR.Ident $ "escape_" <> show lbl
    stackPtr <- getNewVar
    emit $ SetVariable stackPtr (Alloca ty)
    mapM_ (emitCases rt ty label stackPtr vs) cs
    -- crashLbl <- TIR.Ident . ("crash_" <>) . show <$> getNewLabel
    -- emit $ Label crashLbl
    emit . UnsafeRaw $ "call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 6, i64 noundef 6)\n"
    emit . UnsafeRaw $ "call i32 @exit(i32 noundef 1)\n"
    mapM_ (const increaseVarCount) [0..1]
    emit $ Br label
    emit $ Label label
    res <- getNewVar
    emit $ SetVariable res (Load ty Ptr stackPtr)
  where
    emitCases :: LLVMType -> LLVMType -> TIR.Ident -> TIR.Ident -> LLVMValue -> Branch -> CompilerState ()
    emitCases rt ty label stackPtr vs (Branch (MIR.PInj consId cs, t) exp) = do
        emit $ Comment "Inj"
        cons <- gets constructors
        let r = fromJust $ Map.lookup consId cons

        lbl_failPos <- (\x -> TIR.Ident $ "failed_" <> show x) <$> getNewLabel
        lbl_succPos <- (\x -> TIR.Ident $ "success_" <> show x) <$> getNewLabel

        consVal <- getNewVar
        emit $ SetVariable consVal (ExtractValue rt vs 0)

        consCheck <- getNewVar
        emit $ SetVariable consCheck (Icmp LLEq I8 (VIdent consVal I8) (VInteger $ numCI r))
        emit $ BrCond (VIdent consCheck ty) lbl_succPos lbl_failPos
        emit $ Label lbl_succPos

        castPtr <- getNewVar
        casted <- getNewVar
        emit $ SetVariable castPtr (Alloca rt)
        emit $ Store rt vs Ptr castPtr
        emit $ SetVariable casted (Load (CustomType (coerce consId)) Ptr castPtr)
        val <- exprToValue exp
        enumerateOneM_
            (\i c -> do
                case c of
                    PVar x      -> do
                        emit . Comment $ "ident " <> show x
                        emit $ SetVariable (fst x) (ExtractValue (CustomType (coerce consId)) (VIdent casted Ptr) i)
                    PLit (l, t) -> undefined
                    PInj id ps  -> undefined
                    PCatch      -> pure()
                    PEnum id    -> undefined
                --case c of
                --    CIdent x -> do
                --        emit . Comment $ "ident " <> show x
                --        emit $ SetVariable x (ExtractValue (CustomType (fst consId)) (VIdent casted Ptr) i)
                --        emit $ Store ty val Ptr stackPtr
                --    CCons x cs -> error "nested constructor"
                --    CLit l -> do
                --        testVar <- getNewVar
                --        emit $ SetVariable testVar (ExtractValue (CustomType (fst consId)) (VIdent casted Ptr) i)
                --        case l of
                --            LInt l -> emit $ Icmp LLEq I64 (VIdent testVar Ptr) (VInteger l)
                --            LChar c -> emit $ Icmp LLEq I8 (VIdent testVar Ptr) (VChar c)
                --    CCatch -> emit . Comment $ "Catch all"
            )
            cs
        emit $ Store ty val Ptr stackPtr
        emit $ Br label
        emit $ Label lbl_failPos
    emitCases rt ty label stackPtr vs (Branch (MIR.PLit i, _) exp) = do
        emit $ Comment "Plit"
        let i' = case i of
                (MIR.LInt i, _)  -> VInteger i
                (MIR.LChar i, _) -> VChar i
        ns <- getNewVar
        lbl_failPos <- (\x -> TIR.Ident $ "failed_" <> show x) <$> getNewLabel
        lbl_succPos <- (\x -> TIR.Ident $ "success_" <> show x) <$> getNewLabel
        emit $ SetVariable ns (Icmp LLEq ty vs i')
        emit $ BrCond (VIdent ns ty) lbl_succPos lbl_failPos
        emit $ Label lbl_succPos
        val <- exprToValue exp
        emit $ Store ty val Ptr stackPtr
        emit $ Br label
        emit $ Label lbl_failPos
    emitCases rt ty label stackPtr vs (Branch (MIR.PVar (id,_), _) exp) = do
        emit $ Comment "Pvar"
        -- //TODO this is pretty disgusting and would heavily benefit from a rewrite
        valPtr <- getNewVar
        emit $ SetVariable valPtr (Alloca rt)
        emit $ Store rt vs Ptr valPtr
        emit $ SetVariable id (Load rt Ptr valPtr)
        val <- exprToValue exp
        emit $ Store ty val Ptr stackPtr
        emit $ Br label
        lbl_failPos <- (\x -> TIR.Ident $ "failed_" <> show x) <$> getNewLabel
        emit $ Label lbl_failPos
    emitCases rt ty label stackPtr vs (Branch (MIR.PEnum id, _) exp) = do
        emit $ Comment "Penum"
        val <- exprToValue exp
        emit $ Store ty val Ptr stackPtr
        emit $ Br label
        lbl_failPos <- (\x -> TIR.Ident $ "failed_" <> show x) <$> getNewLabel
        emit $ Label lbl_failPos
    emitCases _ ty label stackPtr _ (Branch (MIR.PCatch, _) exp) = do
        emit $ Comment "Pcatch"
        val <- exprToValue exp
        emit $ Store ty val Ptr stackPtr
        emit $ Br label
        lbl_failPos <- (\x -> TIR.Ident $ "failed_" <> show x) <$> getNewLabel
        emit $ Label lbl_failPos

--emitLet :: Bind -> Exp -> CompilerState ()
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

emitApp :: MIR.Type -> ExpT -> ExpT -> CompilerState ()
emitApp rt e1 e2 = appEmitter e1 e2 []
  where
    appEmitter :: ExpT -> ExpT -> [ExpT] -> CompilerState ()
    appEmitter e1 e2 stack = do
        let newStack = e2 : stack
        case e1 of
            (MIR.EApp e1' e2', _) -> appEmitter e1' e2' newStack
            (MIR.EVar name, t) -> do
                args <- traverse exprToValue newStack
                vs <- getNewVar
                funcs <- gets functions
                consts <- gets constructors
                let visibility =
                        fromMaybe Local $
                            Global <$ Map.lookup name consts
                            <|>
                            Global <$ Map.lookup (name, t) funcs
                    -- this piece of code could probably be improved, i.e remove the double `const Global`
                    args' = map (first valueGetType . dupe) args
                    call = Call FastCC (type2LlvmType rt) visibility name args'
                emit $ Comment $ show rt
                emit $ SetVariable vs call
            x -> error $ "The unspeakable happened: " <> show x

emitIdent :: TIR.Ident -> CompilerState ()
emitIdent id = do
    -- !!this should never happen!!
    emit $ Comment "This should not have happened!"
    emit $ Variable id
    emit $ UnsafeRaw "\n"

emitLit :: MIR.Lit -> CompilerState ()
emitLit i = do
    -- !!this should never happen!!
    let (i', t) = case i of
            (MIR.LInt i'')  -> (VInteger i'', I64)
            (MIR.LChar i'') -> (VChar i'', I8)
    varCount <- getNewVar
    emit $ Comment "This should not have happened!"
    emit $ SetVariable varCount (Add t i' (VInteger 0))

emitAdd :: MIR.Type -> ExpT -> ExpT -> CompilerState ()
emitAdd t e1 e2 = do
    v1 <- exprToValue e1
    v2 <- exprToValue e2
    v <- getNewVar
    emit $ SetVariable v (Add (type2LlvmType t) v1 v2)

emitSub :: MIR.Type -> ExpT -> ExpT -> CompilerState ()
emitSub t e1 e2 = do
    v1 <- exprToValue e1
    v2 <- exprToValue e2
    v <- getNewVar
    emit $ SetVariable v (Sub (type2LlvmType t) v1 v2)

exprToValue :: ExpT -> CompilerState LLVMValue
exprToValue = \case
    (MIR.ELit i, t) -> pure $ case i of
        (MIR.LInt i)  -> VInteger i
        (MIR.LChar i) -> VChar i
    (MIR.EVar name, t) -> do
        funcs <- gets functions
        cons <- gets constructors
        let res = Map.lookup (name, t) funcs
                  <|>
                  (\c -> FunctionInfo { numArgs = numArgsCI c
                                     , arguments = argumentsCI c} )
                    <$> Map.lookup name cons
        case res of
            Just fi -> do
                if numArgs fi == 0
                    then do
                        vc <- getNewVar
                        emit $
                            SetVariable
                                vc
                                (Call FastCC (type2LlvmType t) Global name [])
                        pure $ VIdent vc (type2LlvmType t)
                    else pure $ VFunction name Global (type2LlvmType t)
            Nothing -> pure $ VIdent name (type2LlvmType t)
    e -> do
        compileExp e
        v <- getVarCount
        pure $ VIdent (TIR.Ident $ show v) (getType e)

type2LlvmType :: MIR.Type -> LLVMType
type2LlvmType (MIR.TLit id@(TIR.Ident name)) = case name of
    "Int" -> I64
    _     -> CustomType id
type2LlvmType (MIR.TFun t xs)           = do
    let (t', xs') = function2LLVMType xs [type2LlvmType t]
    Function t' xs'
  where
    function2LLVMType :: Type -> [LLVMType] -> (LLVMType, [LLVMType])
    function2LLVMType (TFun t xs) s = function2LLVMType xs (type2LlvmType t : s)
    function2LLVMType x s           = (type2LlvmType x, s)

getType :: ExpT -> LLVMType
getType (_, t)     = type2LlvmType t

extractTypeName :: MIR.Type -> TIR.Ident
extractTypeName (MIR.TLit id) = id
extractTypeName (MIR.TFun t xs) = let (TIR.Ident i) = extractTypeName t
                                      (TIR.Ident is) = extractTypeName xs
                                      in TIR.Ident $ i <> "_$_" <> is

valueGetType :: LLVMValue -> LLVMType
valueGetType (VInteger _)      = I64
valueGetType (VChar _)         = I8
valueGetType (VIdent _ t)      = t
valueGetType (VConstant s)     = Array (fromIntegral $ length s) I8
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

enumerateOneM_ :: Monad m => (Integer -> a -> m b) -> [a] -> m ()
enumerateOneM_ f = foldM_ (\i a -> f i a >> pure (i + 1)) 1
