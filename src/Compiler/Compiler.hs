module Compiler.Compiler where

import           Control.Applicative     (Applicative)
import           Control.Monad.Except    (Except, MonadError (throwError),
                                          liftEither)
import           Data.Either.Combinators (maybeToRight)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Grammar.Abs
import           Grammar.Print           (printTree)
import Grammar.Par (myLexer,pProgram)
import System.Exit (exitFailure)

compileFile :: String -> IO ()
compileFile file = do 
    input <- readFile file
    case pProgram (myLexer input) of 
        Left err -> do
            putStrLn "SYNTAX ERROR"
            putStrLn err
            exitFailure
        Right cor -> do
            putStrLn $ printTree cor
            compile cor

-- data Compiler = Compiler 
--     { data :: [LLVMIr] }
-- 
-- data LLVMIr = LLVMIr



compile :: Program -> IO ()
compile p = print "hej"