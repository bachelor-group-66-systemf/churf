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
import Monomorphizer.MonomorphizerIr as MIR (Def (DBind, DData), Program (..))

{- | Compiles an AST and produces a LLVM Ir string.
  An easy way to actually "compile" this output is to
  Simply pipe it to LLI
-}
generateCode :: MIR.Program -> Err String
generateCode (MIR.Program scs) = do
  let codegen = initCodeGenerator scs
  llvmIrToString . instructions <$> execStateT (compileScs (sortBy lowData scs)) codegen

lowData :: Def -> Def -> Ordering
lowData (DData _) (DBind _) = LT
lowData (DBind _) (DData _) = GT
lowData _ _ = EQ