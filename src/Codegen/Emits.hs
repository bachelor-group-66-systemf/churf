{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Codegen.Emits where

import           Codegen.Auxillary
import           Codegen.CompilerState
import           Codegen.LlvmIr                as LIR
import           Control.Applicative           ((<|>))
import           Control.Monad                 (forM_, when)
import           Control.Monad.State           (gets, modify)
import qualified Data.Bifunctor                as BI
import           Data.Char                     (ord)
import           Data.Coerce                   (coerce)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust, fromMaybe)
import           Data.Tuple.Extra              (dupe, first, second)
import           Monomorphizer.MonomorphizerIr (Ident (..))
import           Monomorphizer.MonomorphizerIr as MIR

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


            -- Higher order functions uses ptr
            let f t = case t of
                          TFun _ _ -> Ptr
                          _        -> type2LlvmType t

            let x = BI.second f <$> argumentsCI ci

            emit $ Define FastCC t' id x

            -- Dereference: ptr → i64(i64)*
            forM_ x $ \(x@(Ident s), t) -> case t of
                                   Ptr -> do
                                       let t_f =  fromMaybe (error "Something wrong!")
                                                  $ lookup x ci.argumentsCI
                                       emit $ SetVariable (Ident $ s ++ "_deref")
                                            $ Load (type2LlvmType t_f) Ptr x
                                       undefined
                                   _   -> pure ()

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
            cTypes <- gets customTypes

            enumerateOneM_
                ( \i (Ident arg_n, arg_t) -> do
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
                    case Map.lookup arg_t' cTypes of
                        Just s -> do
                            emit $ Comment "Malloc and store"
                            heapPtr <- getNewVar
                            useGc <- gets gcEnabled
                            emit $ SetVariable heapPtr (if useGc then GcMalloc s else Malloc s)
                            emit $ Store arg_t' (VIdent (Ident arg_n) arg_t') Ptr heapPtr
                            emit $ Store (Ref arg_t') (VIdent heapPtr arg_t') Ptr elemPtr
                        Nothing -> do
                            emit $ Comment "Just store"
                            emit $ Store arg_t' (VIdent (Ident arg_n) arg_t') Ptr elemPtr
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
compileScs (MIR.DBind (MIR.Bind (name, t) args exp) : xs) = do
    let t_return = type2LlvmType . last . flattenType $ t
    emit $ UnsafeRaw "\n"
    emit . Comment $ show name <> ": " <> show exp
    let args' = map (second type2LlvmType) args
    emit $ Define FastCC t_return name args'
    useGc <- gets gcEnabled
    when (name == "main") (mapM_ emit (firstMainContent useGc))
    functionBody <- exprToValue exp
    if name == "main"
        then mapM_ emit $ lastMainContent useGc functionBody
        else emit $ Ret t_return functionBody
    emit DefineEnd
    modify $ \s -> s{variableCount = 0}
    compileScs xs
compileScs (MIR.DData (MIR.Data typ ts) : xs) = do
    let (Ident outer_id) = extractTypeName typ
    -- //TODO this could be extracted from the customTypes map
    let variantTypes fi = init $ map type2LlvmType (flattenType fi)
    let biggestVariant = 7 + maximum (sum . (\(Inj _ fi) -> typeByteSize <$> variantTypes fi) <$> ts)
    emit $ LIR.Type (Ident outer_id) [I8, Array biggestVariant I8]
    typeSets <- gets customTypes
    mapM_
        ( \(Inj inner_id fi) -> do
            let types = (\s -> if Map.member s typeSets then Ref s else s) <$> variantTypes fi
            emit $ LIR.Type inner_id (I8 : types)
        )
        ts
    compileScs xs

firstMainContent :: Bool -> [LLVMIr]
firstMainContent True =
    [ UnsafeRaw "%prof = call ptr @cheap_the()\n"
    , UnsafeRaw "call void @cheap_set_profiler(ptr %prof, i1 true)\n"
    , UnsafeRaw "call void @cheap_profiler_log_options(ptr %prof, i64 30)\n"
    , UnsafeRaw "call void @cheap_init()\n"
    ]
firstMainContent False = []

lastMainContent :: Bool -> LLVMValue -> [LLVMIr]
lastMainContent True var =
    [ UnsafeRaw $
        "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef " <> toIr var <> ")\n"
    , UnsafeRaw "call void @cheap_dispose()\n"
    , Ret I64 (VInteger 0)
    ]
lastMainContent False var =
    [ UnsafeRaw $
        "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef " <> toIr var <> ")\n"
    , Ret I64 (VInteger 0)
    ]

compileExp :: T Exp -> CompilerState ()
compileExp (MIR.ELit lit, _t)   = emitLit lit
compileExp (MIR.EAdd e1 e2, t)  = emitAdd t e1 e2
compileExp (MIR.EVar name, _t)  = emitIdent name
compileExp (MIR.EApp e1 e2, t)  = emitApp t e1 e2
compileExp (MIR.ELet bind e, _) = emitLet bind e
compileExp (MIR.ECase e cs, t)  = emitECased t e (map (t,) cs)
-- compileExp (MIR.EVarCxt name cxt) = emitEVarCxt name cxt

emitLet :: MIR.Bind -> T Exp -> CompilerState ()
emitLet (MIR.Bind id [] innerExp) e = do
    evaled <- exprToValue innerExp
    tempVar <- getNewVar
    let t = type2LlvmType . snd $ innerExp
    emit $ SetVariable tempVar (Alloca t)
    emit $ Store (type2LlvmType . snd $ innerExp) evaled Ptr tempVar
    emit $ SetVariable (fst id) (Load t Ptr tempVar)
    compileExp e
emitLet b _ = error $ "Non empty argument list in let-bind " <> show b

emitECased :: MIR.Type -> T Exp -> [(MIR.Type, Branch)] -> CompilerState ()
emitECased t e cases = do
    let cs = snd <$> cases
    let ty = type2LlvmType t
    let rt = type2LlvmType (snd e)
    vs <- exprToValue e
    lbl <- getNewLabel
    let label = Ident $ "escape_" <> show lbl
    stackPtr <- getNewVar
    emit $ SetVariable stackPtr (Alloca ty)
    mapM_ (emitCases rt ty label stackPtr vs) cs
    -- crashLbl <- TIR.Ident . ("crash_" <>) . show <$> getNewLabel
    -- emit $ Label crashLbl
    var_num <- getVarCount
    emit . UnsafeRaw $ "call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef " <> show var_num <> ", i64 noundef 6)\n"
    useGc <- gets gcEnabled
    when useGc (emit . UnsafeRaw $ "call void @cheap_dispose()\n")
    emit . UnsafeRaw $ "call i32 @exit(i32 noundef 1)\n"
    mapM_ (const increaseVarCount) [0 .. 1]
    emit $ Br label
    emit $ Label label
    res <- getNewVar
    emit $ SetVariable res (Load ty Ptr stackPtr)
  where
    emitCases :: LLVMType -> LLVMType -> Ident -> Ident -> LLVMValue -> Branch -> CompilerState ()
    emitCases rt ty label stackPtr vs (Branch (MIR.PInj consId cs, _t) exp) = do
        emit $ Comment "Inj"
        cons <- gets constructors
        let r = fromJust $ Map.lookup consId cons

        lbl_failPos <- (\x -> Ident $ "failed_" <> show x) <$> getNewLabel
        lbl_succPos <- (\x -> Ident $ "success_" <> show x) <$> getNewLabel

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
        enumerateOneM_
            ( \i (c, t) -> do
                case c of
                    PVar x -> do
                        let topT' = type2LlvmType t
                        let botT' = CustomType (coerce consId)
                        emit . Comment $ "ident " <> toIr topT'
                        cTypes <- gets customTypes
                        if Map.member topT' cTypes
                            then do
                                deref <- getNewVar
                                emit $ SetVariable deref (ExtractValue botT' (VIdent casted Ptr) i)
                                emit $ SetVariable x (Load topT' Ptr deref)
                            else emit $ SetVariable x (ExtractValue botT' (VIdent casted Ptr) i)
                    PLit _l -> error "Nested pattern matching to be implemented"
                    PInj _id _ps -> error "Nested pattern matching to be implemented"
                    PCatch -> pure ()
                    PEnum _id -> error "Nested pattern matching to be implemented"
            )
            cs
        val <- exprToValue exp
        emit $ Store ty val Ptr stackPtr
        emit $ Br label
        emit $ Label lbl_failPos
    emitCases _rt ty label stackPtr vs (Branch (MIR.PLit i, t) exp) = do
        emit $ Comment "Plit"
        let i' = case i of
                MIR.LInt i  -> VInteger i
                MIR.LChar i -> VChar (ord i)
        ns <- getNewVar
        lbl_failPos <- (\x -> Ident $ "failed_" <> show x) <$> getNewLabel
        lbl_succPos <- (\x -> Ident $ "success_" <> show x) <$> getNewLabel
        emit $ SetVariable ns (Icmp LLEq (type2LlvmType t) vs i')
        emit $ BrCond (VIdent ns ty) lbl_succPos lbl_failPos
        emit $ Label lbl_succPos
        val <- exprToValue exp
        emit $ Store ty val Ptr stackPtr
        emit $ Br label
        emit $ Label lbl_failPos
    emitCases rt ty label stackPtr vs (Branch (MIR.PVar id, _) exp) = do
        emit $ Comment "Pvar"
        -- //TODO this is pretty disgusting and would heavily benefit from a rewrite
        valPtr <- getNewVar
        emit $ SetVariable valPtr (Alloca rt)
        emit $ Store rt vs Ptr valPtr
        emit $ SetVariable id (Load rt Ptr valPtr)
        val <- exprToValue exp
        emit $ Store ty val Ptr stackPtr
        emit $ Br label
        lbl_failPos <- (\x -> Ident $ "failed_" <> show x) <$> getNewLabel
        emit $ Label lbl_failPos
    emitCases rt ty label stackPtr vs (Branch (MIR.PEnum (Ident "True"), t) exp) = do
        emitCases rt ty label stackPtr vs (Branch (MIR.PLit (MIR.LInt 1), TLit "Bool") exp)
    emitCases rt ty label stackPtr vs (Branch (MIR.PEnum (Ident "False"), _) exp) = do
        emitCases rt ty label stackPtr vs (Branch (MIR.PLit (MIR.LInt 0), TLit "Bool") exp)
    emitCases rt ty label stackPtr vs (Branch (MIR.PEnum consId, _) exp) = do
        -- //TODO Penum wrong, acts as a catch all
        emit $ Comment "Penum"
        cons <- gets constructors
        let r = fromJust $ Map.lookup consId cons

        lbl_failPos <- (\x -> Ident $ "failed_" <> show x) <$> getNewLabel
        lbl_succPos <- (\x -> Ident $ "success_" <> show x) <$> getNewLabel

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
        emit $ Store ty val Ptr stackPtr
        emit $ Br label
        emit $ Label lbl_failPos
    emitCases _ ty label stackPtr _ (Branch (MIR.PCatch, _) exp) = do
        emit $ Comment "Pcatch"
        val <- exprToValue exp
        emit $ Store ty val Ptr stackPtr
        emit $ Br label
        lbl_failPos <- (\x -> Ident $ "failed_" <> show x) <$> getNewLabel
        emit $ Label lbl_failPos

emitApp :: MIR.Type -> T Exp -> T Exp -> CompilerState ()
emitApp rt e1 e2 = appEmitter e1 e2 []
  where
    appEmitter :: T Exp -> T Exp -> [T Exp] -> CompilerState ()
    appEmitter e1 e2 stack = do
        let newStack = e2 : stack
        case e1 of
            (MIR.EApp e1' e2', _) -> appEmitter e1' e2' newStack
            (MIR.EVarCxt name cxt, t) -> do
                args <- traverse exprToValue newStack
                vs <- getNewVar
                funcs <- gets functions
                consts <- gets constructors
                let visibility =
                        fromMaybe Local $
                            Global <$ Map.lookup name consts
                                <|> Global <$ Map.lookup (name, t) funcs
                    -- this piece of code could probably be improved, i.e remove the double `const Global`
                    args' = map (first valueGetType . dupe) args
                let call =
                        case name of
                            Ident ('l' : 't' : '$' : _) -> Icmp LLSlt I64 (snd (head args')) (snd (args' !! 1))
                            Ident s -> Call FastCC (type2LlvmType rt) visibility (Ident $ s ++ "_deref") (args') -- TODO need local vars!
                emit $ Comment $ show rt
                emit $ SetVariable vs call

            (MIR.EVar name, t) -> do
                args <- traverse exprToValue newStack
                vs <- getNewVar
                funcs <- gets functions
                consts <- gets constructors
                let visibility =
                        fromMaybe Local $
                            Global <$ Map.lookup name consts
                                <|> Global <$ Map.lookup (name, t) funcs
                    -- this piece of code could probably be improved, i.e remove the double `const Global`
                    args' = map (first valueGetType . dupe) args
                let call =
                        case name of
                            Ident ('l' : 't' : '$' : _) -> Icmp LLSlt I64 (snd (head args')) (snd (args' !! 1))
                            _ -> Call FastCC (type2LlvmType rt) visibility name args'
                emit $ Comment $ show rt
                emit $ SetVariable vs call
            x -> error $ "The unspeakable happened: " <> show x

emitIdent :: Ident -> CompilerState ()
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
            (MIR.LChar i'') -> (VChar $ ord i'', I8)
    varCount <- getNewVar
    emit $ Comment "This should not have happened!"
    emit $ SetVariable varCount (Add t i' (VInteger 0))

emitAdd :: MIR.Type -> T Exp -> T Exp -> CompilerState ()
emitAdd t e1 e2 = do
    v1 <- exprToValue e1
    v2 <- exprToValue e2
    v <- getNewVar
    emit $ SetVariable v (Add (type2LlvmType t) v1 v2)

exprToValue :: T Exp -> CompilerState LLVMValue
exprToValue = \case
    (MIR.ELit i, _t) -> pure $ case i of
        (MIR.LInt i)  -> VInteger i
        (MIR.LChar i) -> VChar $ ord i
    (MIR.EVar (Ident "True"), _t) -> pure $ VInteger 1
    (MIR.EVar (Ident "False"), _t) -> pure $ VInteger 0
    (MIR.EVar name, t) -> do
        funcs <- gets functions
        cons <- gets constructors
        let res =
                Map.lookup (name, t) funcs
                    <|> ( \c ->
                            FunctionInfo
                                { numArgs = numArgsCI c
                                , arguments = argumentsCI c
                                }
                        )
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
        pure $ VIdent (Ident $ show v) (getType e)
