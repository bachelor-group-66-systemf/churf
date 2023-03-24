{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen.Codegen where

-- module Codegen.Codegen (generateCode) where

-- | The record used as the code generator state
data CodeGenerator = CodeGenerator
    { instructions  :: [LLVMIr]
    , functions     :: Map MIR.Id FunctionInfo
    , constructors  :: Map MIR.Id ConstructorInfo
    , variableCount :: Integer
    , labelCount    :: Integer
    }

---- | The record used as the code generator state
-- data CodeGenerator = CodeGenerator
--    { instructions  :: [LLVMIr]
--    , functions     :: Map MIR.Id FunctionInfo
--    , constructors  :: Map Ident ConstructorInfo
--    , variableCount :: Integer
--    , labelCount    :: Integer
--    }

---- | A state type synonym
-- type CompilerState a = StateT CodeGenerator Err a

-- data FunctionInfo = FunctionInfo
--    { numArgs   :: Int
--    , arguments :: [Id]
--    }
--    deriving (Show)
-- data ConstructorInfo = ConstructorInfo
--    { numArgsCI   :: Int
--    , argumentsCI :: [Id]
--    , numCI       :: Integer
--    }
--    deriving (Show)

---- | Adds a instruction to the CodeGenerator state
-- emit :: LLVMIr -> CompilerState ()
-- emit l = modify $ \t -> t{instructions = Auxiliary.snoc l $ instructions t}

---- | Increases the variable counter in the CodeGenerator state
-- increaseVarCount :: CompilerState ()
-- increaseVarCount = modify $ \t -> t{variableCount = variableCount t + 1}

---- | Returns the variable count from the CodeGenerator state
-- getVarCount :: CompilerState Integer
-- getVarCount = gets variableCount

---- | Increases the variable count and returns it from the CodeGenerator state
-- getNewVar :: CompilerState GA.Ident
-- getNewVar = (GA.Ident . show) <$> (increaseVarCount >> getVarCount)

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
    go (MIR.DData (MIR.Constructor n cons) : xs) =
        do map
                ( \(id, xs) ->
                    ( (coerce id, MIR.TLit (coerce n))
                    , FunctionInfo
                        { numArgs = length (flattenType xs)
                        , arguments = createArgs (flattenType xs)
                        }
                    )
                )
                cons
            <> go xs

-- {- | Produces a map of functions infos from a list of binds,
-- which contains useful data for code generation.
---}
-- getFunctions :: [MIR.Def] -> Map Id FunctionInfo
-- getFunctions bs = Map.fromList $ go bs
--  where
--    go [] = []
--    go (MIR.DBind (MIR.Bind id args _) : xs) =
--        (id, FunctionInfo{numArgs = length args, arguments = args})
--            : go xs
--    go (MIR.DData (MIR.Constructor n cons) : xs) = undefined
--        {-do map
--                ( \(Constructor id xs) ->
--                    ( (id, MIR.TLit n)
--                    , FunctionInfo
--                        { numArgs = length xs
--                        , arguments = createArgs xs
--                        }
--                    )
--                )
--                cons
--            <> go xs-}

{- | Produces a map of functions infos from a list of binds,
 which contains useful data for code generation.
-}
getConstructors :: [MIR.Def] -> Map MIR.Id ConstructorInfo
getConstructors bs = Map.fromList $ go bs
  where
    go []                                        = []
    go (MIR.DData (MIR.Constructor (GA.UIdent n) cons) : xs) =
        do
            fst
                ( foldl
                    ( \(acc, i) (GA.UIdent id, xs) ->
                        ( ( (GA.Ident (n <> "_" <> id), MIR.TLit (coerce n))
                          , ConstructorInfo
                                { numArgsCI = length (flattenType xs)
                                , argumentsCI = createArgs (flattenType xs)
                                , numCI = i
                                }
                          )
                            : acc
                        , i + 1
                        )
                    )
                    ([], 0)
                    cons
                )
            <> go xs
    go (_ : xs)                                  = go xs

-- {- | Produces a map of functions infos from a list of binds,
-- which contains useful data for code generation.
---}
-- getConstructors :: [MIR.Def] -> Map Ident ConstructorInfo
-- getConstructors bs = Map.fromList $ go bs
--  where
--    go []                                        = []
--    go (MIR.DData (MIR.Constructor n cons) : xs) = undefined
--        {-do
--            fst
--                ( foldl
--                    ( \(acc, i) (GA.Constructor (GA.Ident id) xs) ->
--                        ( ( (GA.Ident (n <> "_" <> id), MIR.TLit (GA.Ident n))
--                          , ConstructorInfo
--                                { numArgsCI = length xs
--                                , argumentsCI = createArgs xs
--                                , numCI = i
--                                }
--                          )
--                            : acc
--                        , i + 1
--                        )
--                    )
--                    ([], 0)
--                    cons
--                )
--            <> go xs-}
--    go (_ : xs)                                  = go xs

-- initCodeGenerator :: [MIR.Def] -> CodeGenerator
-- initCodeGenerator scs =
--    CodeGenerator
--        { instructions = defaultStart
--        , functions = getFunctions scs
--        , constructors = getConstructors scs
--        , variableCount = 0
--        , labelCount = 0
--        }

-- {-
-- run :: Err String -> IO ()
-- run s = do
--    let s' = case s of
--            Right s -> s
--            Left _  -> error "yo"
--    writeFile "output/llvm.ll" s'
--    putStrLn . trim =<< readCreateProcess (shell "lli") s'

compileScs :: [MIR.Def] -> CompilerState ()
compileScs [] = do
    -- as a last step create all the constructors
    -- //TODO maybe merge this with the data type match?
    c <- gets (Map.toList . constructors)
    mapM_
        ( \((id, t), ci) -> do
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

            -- emit $ UnsafeRaw "\n"

            enumerateOneM_
                ( \i (GA.Ident arg_n, arg_t) -> do
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
                    emit $ Store arg_t' (VIdent (GA.Ident arg_n) arg_t') Ptr elemPtr
                )
                (argumentsCI ci)

            -- emit $ UnsafeRaw "\n"

            -- load and return the constructed value
            emit $ Comment "Return the newly constructed value"
            load <- getNewVar
            emit $ SetVariable load (Load t' Ptr top)
            emit $ Ret t' (VIdent load t')
            emit DefineEnd

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
compileScs (MIR.DData (MIR.Constructor (GA.UIdent outer_id) ts) : xs) = do
    let types = BI.second flattenType <$> ts
    let biggestVariant = maximum $ sum . map (typeByteSize . type2LlvmType) <$> (snd <$> types)
    emit $ LIR.Type (coerce outer_id) [I8, Array biggestVariant I8]
    mapM_
        ( \(GA.UIdent inner_id, fi) -> do
            emit $ LIR.Type (GA.Ident $ outer_id <> "_" <> inner_id) (I8 : map type2LlvmType fi)
        )
        types
    compileScs xs

-- mainContent :: LLVMValue -> [LLVMIr]
-- mainContent var =
--    [ UnsafeRaw $
--        -- "%2 = alloca %Craig\n" <>
--        -- "    store %Craig %1, ptr %2\n" <>
--        -- "    %3 = bitcast %Craig* %2 to i72*\n" <>
--        -- "    %4 = load i72, ptr %3\n" <>
--        -- "    call i32 (ptr, ...) @printf(ptr noundef @.str, i72 noundef %4)\n"
--        "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef " <> toIr var <> ")\n"
--    , -- , SetVariable (GA.Ident "p") (Icmp LLEq I64 (VInteger 2) (VInteger 2))
--      -- , BrCond (VIdent (GA.Ident "p")) (GA.Ident "b_1") (GA.Ident "b_2")
--      -- , Label (GA.Ident "b_1")
--      -- , UnsafeRaw
--      --     "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef 1)\n"
--      -- , Br (GA.Ident "end")
--      -- , Label (GA.Ident "b_2")
--      -- , UnsafeRaw
--      --     "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef 2)\n"
--      -- , Br (GA.Ident "end")
--      -- , Label (GA.Ident "end")
--      Ret I64 (VInteger 0)
--    ]

-- defaultStart :: [LLVMIr]
-- defaultStart =
--    [ UnsafeRaw "target triple = \"x86_64-pc-linux-gnu\"\n"
--    , UnsafeRaw "target datalayout = \"e-m:o-i64:64-f80:128-n8:16:32:64-S128\"\n"
--    , UnsafeRaw "@.str = private unnamed_addr constant [3 x i8] c\"%x\n\", align 1\n"
--    , UnsafeRaw "declare i32 @printf(ptr noalias nocapture, ...)\n"
--    ]

-- compileExp :: ExpT -> CompilerState ()
-- compileExp (MIR.ELit lit,t)     = emitLit lit
-- compileExp (MIR.EAdd e1 e2,t)   = emitAdd t e1 e2
---- compileExp (ESub t e1 e2)  = emitSub t e1 e2
-- compileExp (MIR.EId name,t)     = emitIdent name
-- compileExp (MIR.EApp e1 e2,t)   = emitApp t e1 e2
---- compileExp (EAbs t ti e)   = emitAbs t ti e
-- compileExp (MIR.ELet binds e,t) = undefined -- emitLet binds (fst e)
-- compileExp (MIR.ECase e cs,t)   = emitECased t e (map (t,) cs)

---- go (EMul e1 e2)  = emitMul e1 e2
---- go (EDiv e1 e2)  = emitDiv e1 e2
---- go (EMod e1 e2)  = emitMod e1 e2

--- aux functions ---
emitECased :: MIR.Type -> ExpT -> [(MIR.Type, Injection)] -> CompilerState ()
emitECased t e cases = do
    let cs = snd <$> cases
    let ty = type2LlvmType t
    let rt = type2LlvmType (snd e)
    vs <- exprToValue e
    lbl <- getNewLabel
    let label = GA.Ident $ "escape_" <> show lbl
    stackPtr <- getNewVar
    emit $ SetVariable stackPtr (Alloca ty)
    mapM_ (emitCases rt ty label stackPtr vs) cs
    emit $ Label label
    res <- getNewVar
    emit $ SetVariable res (Load ty Ptr stackPtr)
  where
    emitCases :: LLVMType -> LLVMType -> GA.Ident -> GA.Ident -> LLVMValue -> Injection -> CompilerState ()
    emitCases rt ty label stackPtr vs (Injection (MIR.InitConstructor consId cs, t) exp) = do
        cons <- gets constructors
        let r = fromJust $ Map.lookup (coerce consId, t) cons

--        lbl_failPos <- (\x -> GA.Ident $ "failed_" <> show x) <$> getNewLabel
--        lbl_succPos <- (\x -> GA.Ident $ "success_" <> show x) <$> getNewLabel

--        consVal <- getNewVar
--        emit $ SetVariable consVal (ExtractValue rt vs 0)

--        consCheck <- getNewVar
--        emit $ SetVariable consCheck (Icmp LLEq I8 (VIdent consVal I8) (VInteger $ numCI r))
--        emit $ BrCond (VIdent consCheck ty) lbl_succPos lbl_failPos
--        emit $ Label lbl_succPos

--        castPtr <- getNewVar
--        castedPtr <- getNewVar
--        casted <- getNewVar
--        emit $ SetVariable castPtr (Alloca rt)
--        emit $ Store rt vs Ptr castPtr
--        emit $ SetVariable castedPtr (Bitcast Ptr (VIdent castPtr Ptr) Ptr)
--        emit $ SetVariable casted (Load (CustomType (coerce consId)) Ptr castedPtr)

--        val <- exprToValue exp
--        -- enumerateOneM_
--        --     (\i c -> do
--        --         case c of
--        --             CIdent x -> do
--        --                 emit . Comment $ "ident " <> show x
--        --                 emit $ SetVariable x (ExtractValue (CustomType (fst consId)) (VIdent casted Ptr) i)
--        --                 emit $ Store ty val Ptr stackPtr
--        --             CCons x cs -> error "nested constructor"
--        --             CLit l -> do
--        --                 testVar <- getNewVar
--        --                 emit $ SetVariable testVar (ExtractValue (CustomType (fst consId)) (VIdent casted Ptr) i)
--        --                 case l of
--        --                     LInt l -> emit $ Icmp LLEq I64 (VIdent testVar Ptr) (VInteger l)
--        --                     LChar c -> emit $ Icmp LLEq I8 (VIdent testVar Ptr) (VChar c)
--        --             CCatch -> emit . Comment $ "Catch all"
--        --         emit . Comment $ "return this " <> toIr val
--        --         emit . Comment . show $ c
--        --         emit . Comment . show $ i
--        --     )
--        --     cs
--        -- emit $ Store ty val Ptr stackPtr
--        emit $ Br label
--        emit $ Label lbl_failPos
--    emitCases rt ty label stackPtr vs (Injection (MIR.InitLit i, _) exp) = do
--        let i' = case i of
--                GA.LInt i  -> VInteger i
--                GA.LChar i -> VChar i
--        ns <- getNewVar
--        lbl_failPos <- (\x -> GA.Ident $ "failed_" <> show x) <$> getNewLabel
--        lbl_succPos <- (\x -> GA.Ident $ "success_" <> show x) <$> getNewLabel
--        emit $ SetVariable ns (Icmp LLEq ty vs i')
--        emit $ BrCond (VIdent ns ty) lbl_succPos lbl_failPos
--        emit $ Label lbl_succPos
--        val <- exprToValue exp
--        emit $ Store ty val Ptr stackPtr
--        emit $ Br label
--        emit $ Label lbl_failPos
----     emitCases rt ty label stackPtr vs (Injection (MIR.CIdent id) exp) = do
----         -- //TODO this is pretty disgusting and would heavily benefit from a rewrite
----         valPtr <- getNewVar
----         emit $ SetVariable valPtr (Alloca rt)
----         emit $ Store rt vs Ptr valPtr
----         emit $ SetVariable id (Load rt Ptr valPtr)
----         increaseVarCount
----         val <- exprToValue (fst exp)
----         emit $ Store ty val Ptr stackPtr
----         emit $ Br label
--    emitCases _ ty label stackPtr _ (Injection (MIR.InitCatch, _) exp) = do
--        val <- exprToValue exp
--        emit $ Store ty val Ptr stackPtr
--        emit $ Br label

----emitLet :: Bind -> Exp -> CompilerState ()
-- emitLet xs e = do
--    emit $
--        Comment $
--            concat
--                [ "ELet ("
--                , show xs
--                , " = "
--                , show e
--                , ") is not implemented!"
--                ]

emitApp :: MIR.Type -> ExpT -> ExpT -> CompilerState ()
emitApp t e1 e2 = appEmitter e1 e2 []
  where
    appEmitter :: ExpT -> ExpT -> [ExpT] -> CompilerState ()
    appEmitter e1 e2 stack = do
        let newStack = e2 : stack
        case e1 of
            (MIR.EApp e1' e2', t) -> appEmitter e1' e2' newStack
            (MIR.EId name, t) -> do
                args <- traverse exprToValue newStack
                vs <- getNewVar
                funcs <- gets functions
                consts <- gets constructors
                let visibility =
                        fromMaybe Local $
                            Global <$ Map.lookup (name, t) consts
                                <|> Global <$ Map.lookup (name,t) funcs
                    -- this piece of code could probably be improved, i.e remove the double `const Global`
                    args' = map (first valueGetType . dupe) args
                    call = Call FastCC (type2LlvmType t) visibility name args'
                emit $ SetVariable vs call
            x -> error $ "The unspeakable happened: " <> show x

-- emitIdent :: GA.Ident -> CompilerState ()
-- emitIdent id = do
--    -- !!this should never happen!!
--    emit $ Comment "This should not have happened!"
--    emit $ Variable id
--    emit $ UnsafeRaw "\n"

-- emitLit :: MIR.Lit -> CompilerState ()
-- emitLit i = do
--    -- !!this should never happen!!
--    let (i', t) = case i of
--            (MIR.LInt i'')  -> (VInteger i'', I64)
--            (MIR.LChar i'') -> (VChar i'', I8)
--    varCount <- getNewVar
--    emit $ Comment "This should not have happened!"
--    emit $ SetVariable (GA.Ident (show varCount)) (Add t i' (VInteger 0))

-- emitAdd :: MIR.Type -> ExpT -> ExpT -> CompilerState ()
-- emitAdd t e1 e2 = do
--    v1 <- exprToValue e1
--    v2 <- exprToValue e2
--    v <- getNewVar
--    emit $ SetVariable (GA.Ident $ show v) (Add (type2LlvmType t) v1 v2)

-- emitSub :: MIR.Type -> ExpT -> ExpT -> CompilerState ()
-- emitSub t e1 e2 = do
--    v1 <- exprToValue e1
--    v2 <- exprToValue e2
--    v <- getNewVar
--    emit $ SetVariable v (Sub (type2LlvmType t) v1 v2)

-- exprToValue :: ExpT -> CompilerState LLVMValue
-- exprToValue = \case
--    (MIR.ELit i, t) -> pure $ case i of
--        (MIR.LInt i)  -> VInteger i
--        (MIR.LChar i) -> VChar i
--    (MIR.EId name, t) -> do
--        funcs <- gets functions
--        case Map.lookup (name, t) funcs of
--            Just fi -> do
--                if numArgs fi == 0
--                    then do
--                        vc <- getNewVar
--                        emit $
--                            SetVariable
--                                vc
--                                (Call FastCC (type2LlvmType t) Global name [])
--                        pure $ VIdent vc (type2LlvmType t)
--                    else pure $ VFunction name Global (type2LlvmType t)
--            Nothing -> pure $ VIdent name (type2LlvmType t)
--    e -> do
--        compileExp e
--        v <- getVarCount
--        pure $ VIdent (GA.Ident $ show v) (getType e)

type2LlvmType :: MIR.Type -> LLVMType
type2LlvmType (MIR.TLit id@(Ident name)) = case name of
    "Int" -> I64
    _     -> CustomType id
type2LlvmType (MIR.TFun t xs)           = do
    let (t', xs') = function2LLVMType xs [type2LlvmType t]
    Function t' xs'
  where
    function2LLVMType :: Type -> [LLVMType] -> (LLVMType, [LLVMType])
    function2LLVMType (TFun t xs) s = function2LLVMType xs (type2LlvmType t : s)
    function2LLVMType x s           = (type2LlvmType x, s)

-- getType :: ExpT -> LLVMType
-- getType (_, t)     = type2LlvmType t

-- valueGetType :: LLVMValue -> LLVMType
-- valueGetType (VInteger _)      = I64
-- valueGetType (VChar _)         = I8
-- valueGetType (VIdent _ t)      = t
-- valueGetType (VConstant s)     = Array (fromIntegral $ length s) I8
-- valueGetType (VFunction _ _ t) = t

-- typeByteSize :: LLVMType -> Integer
-- typeByteSize I1             = 1
-- typeByteSize I8             = 1
-- typeByteSize I32            = 4
-- typeByteSize I64            = 8
-- typeByteSize Ptr            = 8
-- typeByteSize (Ref _)        = 8
-- typeByteSize (Function _ _) = 8
-- typeByteSize (Array n t)    = n * typeByteSize t
-- typeByteSize (CustomType _) = 8

-- enumerateOneM_ :: Monad m => (Integer -> a -> m b) -> [a] -> m ()
-- enumerateOneM_ f = foldM_ (\i a -> f i a >> pure (i + 1)) 1
