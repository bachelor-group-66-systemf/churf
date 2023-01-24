{-# LANGUAGE TemplateHaskell #-}
module Compiler.StandardLLVMLibrary where
import Control.Monad
import Language.Haskell.TH
import Compiler.TH

-- $(genCurries 8)

standardLLVMLibrary :: String
standardLLVMLibrary = $(includeStr "src/Compiler/standard_library.ll")
