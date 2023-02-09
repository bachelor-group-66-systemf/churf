{-# LANGUAGE TemplateHaskell #-}
module Compiler.StandardLLVMLibrary where

import           Compiler.TH (includeStr)

-- | Uses Template Haskell to load our "standard library", which is written in
--   LLVM IR. This library simply includes functions to generate strings from
--   i64s and to print ints.
standardLLVMLibrary :: String
standardLLVMLibrary = $(includeStr "src/Compiler/standard_library.ll")
