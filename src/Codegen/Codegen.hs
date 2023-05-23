{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Codegen.Codegen (generateCode) where

import           Codegen.CompilerState         (CodeGenerator (..),
                                                StructType (inst),
                                                initCodeGenerator)
import           Codegen.Emits                 (compileScs)
import           Codegen.LlvmIr                as LIR (LLVMIr (UnsafeRaw),
                                                       llvmIrToString)
import           Control.Monad.State           (execStateT)
import           Data.Functor                  ((<&>))
import           Data.List                     (sortBy)
import qualified Data.Map                      as Map
import           Grammar.ErrM                  (Err)
import           Monomorphizer.MonomorphizerIr as MIR (Bind (..), Data (..),
                                                       Def (DBind, DData),
                                                       Program (..),
                                                       Type (TLit))
import           TypeChecker.TypeCheckerIr     (Ident (..))

{- | Compiles an AST and produces a LLVM IR string.
  An easy way to actually "compile" this output is to
  Simply pipe it to LLI
-}
generateCode :: MIR.Program -> Bool -> Err String
generateCode (MIR.Program scs) addGc = do
  let tree    = filter (not . detectPrelude) (sortBy lowData scs)
      codegen = initCodeGenerator addGc tree

  -- Append instructions
  execStateT (compileScs tree) codegen <&> \state ->
    llvmIrToString $  defaultStart
                   ++ (if addGc then gcStart else [])
                   ++ map inst (Map.elems state.structTypes)
                   ++ state.instructions

-- | Detects certain types and functions.
--   Used to filter out and replace definitions with LLVM equivelents
detectPrelude :: Def -> Bool
detectPrelude (DData (Data (TLit (Ident "Bool")) _))              = True
detectPrelude (DData (Data (TLit (Ident "Unit")) _))              = True
detectPrelude (DBind (Bind (Ident ('l' : 't' : '$' : _), _) _ _)) = True
detectPrelude _                                                   = False

lowData :: Def -> Def -> Ordering
lowData (DData _) (DBind _) = LT
lowData (DBind _) (DData _) = GT
lowData _ _                 = EQ

defaultStart :: [LLVMIr]
defaultStart =
    [ UnsafeRaw "target triple = \"x86_64-pc-linux-gnu\"\n"
    , UnsafeRaw "target datalayout = \"e-m:o-i64:64-f80:128-n8:16:32:64-S128\"\n"
    , UnsafeRaw "@.str = private unnamed_addr constant [2 x i8] c\"%i\", align 1\n"
    , UnsafeRaw "@.new_line = private unnamed_addr constant [1 x i8] c\"\n\", align 1\n"
    , UnsafeRaw "@.non_exhaustive_patterns = private unnamed_addr constant [41 x i8] c\"Non-exhaustive patterns in case at %i:%i\n\"\n"
    , UnsafeRaw "@.char_print = private unnamed_addr constant [2 x i8] c\"%c\"\n"
    , UnsafeRaw "@.char_print_no_nl = private unnamed_addr constant [3 x i8] c\"%c\0\"\n"
    , UnsafeRaw "@.int_print_no_nl = private unnamed_addr constant [3 x i8] c\"%i\0\"\n"
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
    , UnsafeRaw "declare external void @cheap_profiler_log_options(ptr, i64)\n"
    ]
