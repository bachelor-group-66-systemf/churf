module Codegen.CompilerState where

import Auxiliary (snoc)
import Codegen.Auxillary (type2LlvmType, typeByteSize)
import Codegen.LlvmIr as LIR (LLVMIr (UnsafeRaw), LLVMType)
import Control.Monad.State (
    StateT,
    gets,
    modify,
 )
import Data.Map (Map)
import Data.Map qualified as Map
import Grammar.ErrM (Err)
import Monomorphizer.MonomorphizerIr as MIR
import TypeChecker.TypeCheckerIr qualified as TIR

-- | The record used as the code generator state
data CodeGenerator = CodeGenerator
    { instructions :: [LLVMIr]
    , functions :: Map MIR.Id FunctionInfo
    , customTypes :: Map LLVMType Integer
    , constructors :: Map TIR.Ident ConstructorInfo
    , variableCount :: Integer
    , labelCount :: Integer
    , gcEnabled :: Bool
    }

-- | A state type synonym
type CompilerState a = StateT CodeGenerator Err a

data FunctionInfo = FunctionInfo
    { numArgs :: Int
    , arguments :: [Id]
    }
    deriving (Show)
data ConstructorInfo = ConstructorInfo
    { numArgsCI :: Int
    , argumentsCI :: [Id]
    , numCI :: Integer
    , returnTypeCI :: MIR.Type
    }
    deriving (Show)

-- | Adds a instruction to the CodeGenerator state
emit :: LLVMIr -> CompilerState ()
emit l = modify $ \t -> t{instructions = Auxiliary.snoc l $ instructions t}

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

initCodeGenerator :: Bool -> [MIR.Def] -> CodeGenerator
initCodeGenerator addGc scs =
    CodeGenerator
        { instructions = defaultStart <> if addGc then gcStart else []
        , functions = getFunctions scs
        , constructors = getConstructors scs
        , customTypes = getTypes scs
        , variableCount = 0
        , labelCount = 0
        , gcEnabled = addGc
        }

defaultStart :: [LLVMIr]
defaultStart =
    [ UnsafeRaw "target triple = \"x86_64-pc-linux-gnu\"\n"
    , UnsafeRaw "target datalayout = \"e-m:o-i64:64-f80:128-n8:16:32:64-S128\"\n"
    , UnsafeRaw "@.str = private unnamed_addr constant [3 x i8] c\"%i\n\", align 1\n"
    , UnsafeRaw "@.non_exhaustive_patterns = private unnamed_addr constant [41 x i8] c\"Non-exhaustive patterns in case at %i:%i\n\"\n"
    , UnsafeRaw "declare i32 @printf(ptr noalias nocapture, ...)\n"
    , UnsafeRaw "declare i32 @exit(i32 noundef)\n"
    , UnsafeRaw "declare ptr @malloc(i32 noundef)\n"
    ]

gcStart :: [LLVMIr]
gcStart =
    [ UnsafeRaw "declare external void @cheap_init()\n"
    , UnsafeRaw "declare external ptr @cheap_alloc(i64)\n"
    , UnsafeRaw "declare external void @cheap_dispose()\n"
    , UnsafeRaw "declare external ptr @cheap_the()\n"
    , UnsafeRaw "declare external void @cheap_set_profiler(ptr, i1)\n"
    ]