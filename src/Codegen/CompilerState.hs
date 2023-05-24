{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Codegen.CompilerState where

import           Auxiliary                     (snoc)
import           Codegen.Auxillary             (type2LlvmType, typeByteSize)
import           Codegen.LlvmIr                as LIR (LLVMIr (SetVariable, Type),
                                                       LLVMType (CustomType, Function, I64, Ptr),
                                                       LLVMValue (VFunction, VIdent),
                                                       Visibility (Global),
                                                       typeOf)
import           Control.Monad.State           (StateT, gets, modify, void)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Grammar.ErrM                  (Err)
import           Monomorphizer.MonomorphizerIr (Ident (..), Inj (..), T,
                                                flattenType)
import qualified Monomorphizer.MonomorphizerIr as MIR
import qualified TypeChecker.TypeCheckerIr     as TIR

-- | The record used as the code generator state
data CodeGenerator = CodeGenerator
    { instructions  :: [LLVMIr]
    , functions     :: Map (T Ident) FunctionInfo
    , customTypes   :: Map LLVMType Integer
    , constructors  :: Map Ident ConstructorInfo
    , variableCount :: Integer
    , labelCount    :: Integer
    , gcEnabled     :: Bool
    , structTypes   :: Map Ident StructType
    -- ^ Custom stucture types
    , locals        :: Map Ident (LLVMType, LLVMValue)
    -- ^ Arguments and variables in local environment
    , globals       :: Map Ident (LLVMType, LLVMValue)
    }

data StructType = StructType
    { ptr  :: LLVMType
    , typs :: [LLVMType]
    , inst :: LLVMIr
    }

-- | A state type synonym
type CompilerState a = StateT CodeGenerator Err a

data FunctionInfo = FunctionInfo
    { numArgs   :: Int
    , arguments :: [T Ident]
    }
    deriving (Show)
data ConstructorInfo = ConstructorInfo
    { numArgsCI    :: Int
    , argumentsCI  :: [T Ident]
    , numCI        :: Integer
    , returnTypeCI :: MIR.Type
    }
    deriving (Show)


addStructType_ :: Ident -> [LLVMType] -> CompilerState ()
addStructType_ = fmap void . addStructType

addStructType :: Ident -> [LLVMType] -> CompilerState LLVMType
addStructType x ts = do
    modify $ \s -> s { structTypes = Map.insert x struct s.structTypes }
    pure t
  where
    struct = StructType
        { ptr  = t
        , typs = ts
        , inst = Type x ts
        }
    t = CustomType x

-- | Adds a instruction to the CodeGenerator state
emit :: LLVMIr -> CompilerState ()

-- Add variable to environment
emit l@(SetVariable x _) = modify $ \t ->
    t { instructions = Auxiliary.snoc l t.instructions
      , locals       = Map.insert x (tl, VIdent x tl)
                       t.locals
      }
  where
    tl = typeOf l

emit l = modify $ \t -> t { instructions = Auxiliary.snoc l t.instructions }

-- | Increases the variable counter in the CodeGenerator state
increaseVarCount :: CompilerState ()
increaseVarCount = modify $ \t -> t{variableCount = variableCount t + 1}

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
getFunctions :: [MIR.Def] -> Map (T Ident) FunctionInfo
getFunctions bs = Map.fromList $ go bs
  where
    go [] = []
    go (MIR.DBind (MIR.Bind id args _) : xs) =
        (id, FunctionInfo { numArgs = length args
                          , arguments = args
                          }
        )
        : go xs
    go (_ : xs) = go xs

createArgs :: [MIR.Type] -> [T Ident]
createArgs xs = fst $ foldl (\(acc, l) t -> (acc ++ [(TIR.Ident ("arg_" <> show l), t)], l + 1)) ([], 0) xs

{- | Produces a map of functions infos from a list of binds,
 which contains useful data for code generation.
-}
getConstructors :: [MIR.Def] -> Map TIR.Ident ConstructorInfo
getConstructors bs = Map.fromList $ go bs
  where
    go [] = []
    go (MIR.DData (MIR.Data t cons) : xs) =
        fst
            ( foldl
                ( \(acc, i) (Inj id xs) ->
                    ( ( id
                      , ConstructorInfo
                            { numArgsCI = length (init . flattenType $ xs)
                            , argumentsCI = createArgs (init . flattenType $ xs)
                            , numCI = i
                            , returnTypeCI = t -- last . flattenType $ xs
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
    go (_ : xs) = go xs

getTypes :: [MIR.Def] -> Map LLVMType Integer
getTypes bs = Map.fromList $ go bs
  where
    go [] = []
    go (MIR.DData (MIR.Data t ts) : xs) = (type2LlvmType t, biggestVariant ts) : go xs
    go (_ : xs) = go xs
    variantTypes fi = init $ map type2LlvmType (flattenType fi)
    biggestVariant ts = 8 + maximum (sum . (\(Inj _ fi) -> typeByteSize <$> variantTypes fi) <$> ts)

getGlobals :: [MIR.Def] -> Map Ident (LLVMType, LLVMValue)
getGlobals scs = Map.fromList [ go b | MIR.DBind b <- scs ]
  where
    go bind = (x, (typ, VFunction x Global typ))
      where
        typ = Function tr $ Ptr : ts
        Function tr ts = type2LlvmType' t

        (x, t) = case bind of
            MIR.Bind xt _ _    -> xt
            MIR.BindC _ xt _ _ -> xt

    -- Higher order function arguments are replaced with ptr
    type2LlvmType' = go []
      where
        go acc = \case
            MIR.TFun (MIR.TFun _ _) t2 -> go (snoc Ptr acc) t2
            MIR.TFun t1             t2 -> go (snoc (type2LlvmType t1) acc) t2
            t                          -> Function (type2LlvmType t) acc




initCodeGenerator :: Bool -> [MIR.Def] -> CodeGenerator
initCodeGenerator addGc scs =
    CodeGenerator
        { instructions = []
        , functions = getFunctions scs
        , constructors = getConstructors scs
        , customTypes = getTypes scs
        , structTypes = mempty
        , variableCount = 0
        , labelCount = 0
        , gcEnabled = addGc
        , locals = mempty
        , globals = getGlobals scs
        }

