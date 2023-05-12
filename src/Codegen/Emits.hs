{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Codegen.Emits where

import           Auxiliary                     (snoc)
import           Codegen.Auxillary
import           Codegen.CompilerState
import           Codegen.LlvmIr                as LIR
import           Control.Applicative           (Applicative (liftA2), (<|>))
import           Control.Monad                 (forM_, when, zipWithM_)
import           Control.Monad.Extra           (whenJust)
import           Control.Monad.State           (gets, modify)
import           Data.Char                     (ord)
import           Data.Coerce                   (coerce)
import           Data.Foldable.Extra           (notNull)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust, fromMaybe, isNothing)
import           Data.Tuple.Extra              (second)
import           Grammar.Print                 (printTree)
import           Monomorphizer.MonomorphizerIr
import Debug.Trace (traceShow)
import Data.List (isPrefixOf)


compileScs :: [Def] -> CompilerState ()
compileScs [] = do
    emit $ UnsafeRaw "\n"
    mapM_ createConstructor =<< gets (Map.toList . constructors)
    -- as a last step create all the constructors
    -- //TODO maybe merge this with the data type match?
  where
    createConstructor (id, ci) = do
            let t  = returnTypeCI ci
                t' = type2LlvmType t
                x  = (mkCxtName, Ptr) :  map (second type2LlvmType) ci.argumentsCI
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

compileScs (DBind bind : xs) = do
    emit $ UnsafeRaw "\n"
    emit . Comment $ show name <> ": " <> show (fst exp)

    Function t_return t_args <- gets $ fst
                                     . fromJust
                                     . Map.lookup name
                                     . globals

    let args' = zip (mkCxtName : map fst args) t_args

    emit $ Define FastCC (if isMain then I64 else t_return) name args'
    modify $ \s -> s  { locals = foldr insertArg s.locals args' }

    -- Dereference ptr arguments
    when (notNull args') $
        forM_ (tail args') $ \(x, t) -> when (t == Ptr) $ do
            let t_deref =
                  let
                    Function t ts = type2LlvmType . fromJust $ lookup x args
                  in
                    Function t (Ptr : ts)

            emit . SetVariable (mkDerefName x)
                 $ Load t_deref Ptr x

    whenJust mcxt loadFreeVars

    gcEnabled <- gets gcEnabled
    when isMain $ mapM_ emit (firstMainContent gcEnabled)

    result <- exprToValue exp

    when isMain $ case t_return of
        I64 -> do 
            emit . UnsafeRaw $
                   "call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef " <> toIr result <> ")\n"
        I8 -> do
            emit . UnsafeRaw $
                   "call i32 (ptr, ...) @printf(ptr noundef @.char_print, i8 noundef " <> toIr result <> ")\n"
        _ -> do
            emit $ Comment "TODO"
    if isMain
        then do 
            emit $ UnsafeRaw "call i32 (ptr, ...) @printf(ptr noundef @.new_line)\n"
            mapM_ emit $ lastMainContent gcEnabled
            emit $ Ret I64 (VInteger 0)
        else emit $ Ret t_return result


    emit DefineEnd
    -- Reset variable count and empty locals
    modify $ \s -> s { variableCount = 0, locals = mempty }
    compileScs xs
  where
    loadFreeVars cxt = do
        emit $ Comment "Load free variables"
        zipWithM_ go cxt' [1 ..]
      where
        go (x, t) i = do
            vc <- getNewVar
            emit . SetVariable vc
                 $ GetElementPtrInbounds (CustomType $ mkClosureName name) Ptr (VIdent mkCxtName Ptr)
                   I32 (VInteger 0) I32 (VInteger i) -- TODO fix indices
            emit . SetVariable x $ Load t Ptr vc
        cxt' = map (second type2LlvmType) cxt

    isMain = name == "main"

    (name, args, exp, mcxt) = case bind of
        Bind (name, _) args exp      -> (name, args, exp, Nothing)
        BindC cxt (name, _) args exp -> (name, args, exp, Just cxt)


    insertArg (x, t) = snoc (x, LocalElem { val = VIdent x t, typ = t })

compileScs (DData (Data typ ts) : xs) = do
    let (Ident outer_id) = extractTypeName typ
    -- //TODO this could be extracted from the customTypes map
    let variantTypes fi = init $ map type2LlvmType (flattenType fi)
    let biggestVariant = 7 + maximum (sum . (\(Inj _ fi) -> typeByteSize <$> variantTypes fi) <$> ts)
    -- Add data type (e.g. %List) to top of the file
    addStructType_ (Ident outer_id) [I8, Array biggestVariant I8]
    typeSets <- gets customTypes
    mapM_
        ( \(Inj inner_id fi) -> do
            let types = (\s -> if Map.member s typeSets then Ref s else s) <$> variantTypes fi
            -- Add constructor type (e.g. %Cons) to top of the file
            addStructType_ inner_id (I8 : types)
        )
        ts
    compileScs xs

firstMainContent :: Bool -> [LLVMIr]
firstMainContent True =
    [ -- UnsafeRaw "%prof = call ptr @cheap_the()\n"
      --     , UnsafeRaw "call void @cheap_set_profiler(ptr %prof, i1 true)\n"
      -- , UnsafeRaw "call void @cheap_profiler_log_options(ptr %prof, i64 30)\n"
      UnsafeRaw "call void @cheap_init()\n"
    ]
firstMainContent False = []

lastMainContent :: Bool -> [LLVMIr]
lastMainContent True = [UnsafeRaw "call void @cheap_dispose()\n"]
lastMainContent False =[]

compileExp :: T Exp -> CompilerState ()
compileExp (ELit lit, _t)   = emitLit lit
compileExp (EAdd e1 e2, t)  = emitAdd t e1 e2
compileExp (EVar name, _t)  = emitIdent name
compileExp (EApp e1 e2, t)  = emitApp t e1 e2
compileExp (ELet bind e, _) = emitLet bind e
compileExp (ECase e cs, t)  = emitECased t e (map (t,) cs)

emitLet :: Bind -> T Exp -> CompilerState ()
emitLet (Bind id [] innerExp) e = do
    evaled <- exprToValue innerExp
    tempVar <- getNewVar
    let t = type2LlvmType . snd $ innerExp
    emit $ SetVariable tempVar (Alloca t)
    emit $ Store (type2LlvmType . snd $ innerExp) evaled Ptr tempVar
    emit $ SetVariable (fst id) (Load t Ptr tempVar)
    compileExp e
emitLet b _ = error $ "Non empty argument list in let-bind " <> show b

emitECased :: Type -> T Exp -> [(Type, Branch)] -> CompilerState ()
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
    emitCases rt ty label stackPtr vs (Branch (PInj consId cs, _t) exp) = do
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
    emitCases _rt ty label stackPtr vs (Branch (PLit i, t) exp) = do
        emit $ Comment "Plit"
        let i' = case i of
                LInt i  -> VInteger i
                LChar i -> VChar (ord i)
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
    emitCases rt ty label stackPtr vs (Branch (PVar id, _) exp) = do
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
    emitCases rt ty label stackPtr vs (Branch (PEnum (Ident "True$Bool"), t) exp) = do
        emitCases rt ty label stackPtr vs (Branch (PLit $ LInt 1, t) exp)
    emitCases rt ty label stackPtr vs (Branch (PEnum (Ident "False$Bool"), t) exp) = do
        emitCases rt ty label stackPtr vs (Branch (PLit (LInt 0), t) exp)
    emitCases rt ty label stackPtr vs (Branch (PEnum (Ident "Unit$Unit"), t) exp) = do
        emitCases rt ty label stackPtr vs (Branch (PLit (LInt 0), t) exp)
    emitCases rt ty label stackPtr vs br@(Branch (PEnum consId, _) exp) = do
        emit $ Comment "Penum"
        cons <- gets constructors
        let r = Map.lookup consId cons
        when (isNothing r) (error $ "Constructor: '" ++ printTree consId ++ "' does not exist in cons state:\n" ++ show cons ++ "\nin pattern\n'" ++ printTree br ++ "'\n")

        lbl_failPos <- (\x -> Ident $ "failed_" <> show x) <$> getNewLabel
        lbl_succPos <- (\x -> Ident $ "success_" <> show x) <$> getNewLabel

        consVal <- getNewVar
        emit $ SetVariable consVal (ExtractValue rt vs 0)

        consCheck <- getNewVar
        emit $ SetVariable consCheck (Icmp LLEq I8 (VIdent consVal I8) (VInteger $ numCI (fromJust r)))
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
    emitCases _ ty label stackPtr _ (Branch (PCatch, _) exp) = do
        emit $ Comment "Pcatch"
        val <- exprToValue exp
        emit $ Store ty val Ptr stackPtr
        emit $ Br label
        lbl_failPos <- (\x -> Ident $ "failed_" <> show x) <$> getNewLabel
        emit $ Label lbl_failPos

preludeFuns :: LLVMIr -> Ident -> LLVMValue -> LLVMValue -> CompilerState LLVMIr
preludeFuns def (Ident xs) arg1 arg2
  | "$langle$$langle$" `isPrefixOf` xs = pure $ Icmp LLSlt I8 arg1 arg2
  | "$langle$" `isPrefixOf` xs =  pure $ Icmp LLSlt I64 arg1 arg2
  | "$minus$" `isPrefixOf` xs = pure $ Sub I64 arg1 arg2
  | "$plus$" `isPrefixOf` xs = pure $ Add I64 arg1 arg2
  | "printChar$" `isPrefixOf` xs = do
        pure . UnsafeRaw $
            "add i16 0,0\n    call void (ptr, ...) @printf(ptr noundef @.char_print_no_nl, i8 noundef " <> toIr arg1 <> ")\n"
  --char_print_no_nl
  | otherwise = pure def

emitApp :: Type -> T Exp -> T Exp -> CompilerState ()
emitApp rt e1 e2 = do
    ((EVar name, t), args) <- go (EApp e1 e2, rt)
    vs <- getNewVar
    funcs <- gets functions
    consts <- gets constructors
    let visibility =
            fromMaybe Local $
                Global <$ Map.lookup name consts
                    <|> Global <$ Map.lookup (name, t) funcs
        -- this piece of code could probably be improved, i.e remove the double `const Global`
    call <- do
            let closure_call LocalElem { typ = Ptr, val } = (mkDerefName name, (Ptr, val) : args)

            (name, args) <- gets $ maybe (name, (Ptr, VNull) : args) closure_call
                                 . lookup name
                                 . locals

            pure $ Call FastCC (type2LlvmType rt) visibility name args
    
    call <- preludeFuns call name (snd (head args)) (snd (args !! 1))

    emit $ Comment $ show (type2LlvmType rt)
    emit $ SetVariable vs call

  where

    go :: T Exp -> CompilerState (T Exp, [(LLVMType, LLVMValue)])
    go et@(e, _) = case e of
      EApp e1 e2@(_, t) -> do
          (x, as) <- go e1
          a <- exprToValue e2
          let t' = type2LlvmType' t
          pure (x, snoc (t', a) as)
      _ -> pure (et, [])

    type2LlvmType' = \case
      TFun _ _ -> Ptr
      t        -> type2LlvmType t

emitIdent :: Ident -> CompilerState ()
emitIdent id = do
    -- !!this should never happen!!
    emit $ Comment "This should not have happened!"
    emit $ Variable id
    emit $ UnsafeRaw "\n"

emitLit :: Lit -> CompilerState ()
emitLit i = do
    -- !!this should never happen!!
    let (i', t) = case i of
            (LInt i'')  -> (VInteger i'', I64)
            (LChar i'') -> (VChar $ ord i'', I8)
    varCount <- getNewVar
    emit $ Comment "This should not have happened!"
    emit $ SetVariable varCount (Add t i' (VInteger 0))

emitAdd :: Type -> T Exp -> T Exp -> CompilerState ()
emitAdd t e1 e2 = do
    v1 <- exprToValue e1
    v2 <- exprToValue e2
    v <- getNewVar
    emit $ SetVariable v (Add (type2LlvmType t) v1 v2)


exprToValue :: T Exp -> CompilerState LLVMValue
exprToValue et@(e, t) = case e of
    ELit (LInt i) -> pure $ VInteger i
    ELit (LChar c) -> pure . VChar $ ord c

    EVar "True$Bool" -> pure $ VInteger 1
    EVar "False$Bool" -> pure $ VInteger 0
    EVar "Unit$Unit" -> pure $ VInteger 0

    EVar name -> gets (Map.lookup name . globals) >>= \case
        Just (typ@(Function _ ts), val) | length ts > 1 -> do
            type_struct <- addStructType (mkClosureName name) [typ]
            emit $ Comment "Allocating structure"
            emit . SetVariable name $ Alloca type_struct
            emit $ Store typ val Ptr name
            pure $ VIdent name Ptr

        Just _ | name == "main" -> do
            vc <- getNewVar
            emit $ SetVariable vc (Call FastCC I64 Global name [])
            pure $ VIdent vc I64


        Just (Function t_return [_], _) -> do
            vc <- getNewVar
            emit $ SetVariable vc (Call FastCC t_return Global name [(Ptr, VNull)])
            pure $ VIdent vc t_return

        Just _ -> error "Bad"

        Nothing -> gets (Map.lookup name . constructors) >>= \case

            Just ConstructorInfo {numArgsCI}
                | numArgsCI == 0 -> do
                    vc <- getNewVar
                    emit $ SetVariable vc call
                    pure $ VIdent vc (type2LlvmType t)
                | otherwise -> pure $ VFunction name Global (type2LlvmType t)
                  where
                    call = Call FastCC (type2LlvmType t) Global name []

            Nothing -> gets $ val
                            . fromJust
                            . lookup name
                            . locals

    EVarC cxt name -> do
        let cxt' = flip map cxt $ \(x, t) -> let t' = type2LlvmType t
                                             in (t', VIdent x t')
        cxt'' <- gets $ (:cxt')
                      . fromJust
                      . Map.lookup name
                      . globals

        -- Create a new type for function pointer and arguments
        type_struct <- addStructType (mkClosureName name) $ map fst cxt''
        emit $ Comment "Allocating structure"
        emit . SetVariable name $ Alloca type_struct

        let ptr_struct = VIdent name Ptr
            storeArg (t, v) i = do
                vc <- getNewVar
                emit . SetVariable vc
                     $ GetElementPtrInbounds type_struct Ptr ptr_struct
                       I32 (VInteger 0) I32 (VInteger i) -- TODO fix indices
                emit $ Store t v Ptr vc

        -- Store arguments in structure
        zipWithM_ storeArg cxt'' [0 ..]
        pure ptr_struct

    _ -> do
        compileExp et
        v <- getVarCount
        pure $ VIdent (Ident $ show v) (getType et)


mkClosureName :: Ident -> Ident
mkClosureName (Ident s) = Ident $ "Closure_" ++ s

mkDerefName :: Ident -> Ident
mkDerefName (Ident s) = Ident $ s ++ "_deref"

mkCxtName :: Ident
mkCxtName = Ident "cxt"
