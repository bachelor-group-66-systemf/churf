{-# LANGUAGE TemplateHaskell #-}
module Compiler.TH where
import Control.Monad
import Language.Haskell.TH
import System.IO.Unsafe(unsafePerformIO)

-- While this is hacky (specifically the use of unsafePerformIO)
-- in this case I think it is fine, as if an invalid string
-- is passed to the function it will fail to compile,
-- which is the intended behavior. This allows us to 
-- import strings (such as our "standard LLVM library")
-- during compile time, removing the need to ship the source for 
-- that with the compiler. 
includeStr :: String -> Q Exp
includeStr file = [|unsafePerformIO $ readFile file|]