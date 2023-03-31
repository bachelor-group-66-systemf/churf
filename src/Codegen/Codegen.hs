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
import Grammar.ErrM (Err)
import Monomorphizer.MonomorphizerIr as MIR (Bind (..), Def (..), Program (..))
import TypeChecker.TypeCheckerIr qualified as TIR

{- | Compiles an AST and produces a LLVM Ir string.
  An easy way to actually "compile" this output is to
  Simply pipe it to LLI
-}
generateCode :: MIR.Program -> Err String
generateCode (MIR.Program scs) = do
    let codegen = initCodeGenerator scs
    let scs' = filter (\x -> case x of MIR.DBind (MIR.Bind (TIR.Ident "$add$Int_Int_Int", _) _ _) -> False; _ -> True) scs
    llvmIrToString . instructions <$> execStateT (compileScs scs') codegen
