module Codegen.Codegen (generateCode) where

import Codegen.CompilerState (
  CodeGenerator (instructions),
  initCodeGenerator,
 )
import Codegen.Emits (compileScs)
import Codegen.LlvmIr as LIR (llvmIrToString)
import Control.Monad.State (
  execStateT,
 )
import Data.List (sortBy)
import Grammar.ErrM (Err)
import Monomorphizer.MonomorphizerIr as MIR (Bind (..), Data (..), Def (DBind, DData), Program (..), Type (TLit))
import TypeChecker.TypeCheckerIr (Ident (..))

{- | Compiles an AST and produces a LLVM Ir string.
  An easy way to actually "compile" this output is to
  Simply pipe it to LLI
-}
generateCode :: MIR.Program -> Bool -> Err String
generateCode (MIR.Program scs) addGc = do
  let tree = filter (not . detectPrelude) (sortBy lowData scs)
  let codegen = initCodeGenerator tree
  llvmIrToString . instructions <$> execStateT (compileScs tree) codegen

detectPrelude :: Def -> Bool
detectPrelude (DData (Data (TLit (Ident "Bool")) _)) = True
detectPrelude (DBind (Bind (Ident ('l' : 't' : '$' : _), _) _ _)) = True
detectPrelude _ = False

lowData :: Def -> Def -> Ordering
lowData (DData _) (DBind _) = LT
lowData (DBind _) (DData _) = GT
lowData _ _ = EQ