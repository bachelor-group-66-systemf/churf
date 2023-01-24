{-# LANGUAGE TemplateHaskell #-}
module Compiler.TH where
import Control.Monad
import Language.Haskell.TH
import System.IO.Unsafe(unsafePerformIO)

includeStr :: String -> Q Exp
includeStr file = do
    let res = unsafePerformIO $ readFile file
    [|res|]