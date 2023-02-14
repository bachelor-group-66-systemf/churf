{-# LANGUAGE LambdaCase #-}
module Main where

import           Grammar.Par        (myLexer, pProgram)
import           Grammar.Print      (printTree)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import TypeChecker.TypeChecker (typecheck)
import Renamer.Renamer (rename)

main :: IO ()
main = getArgs >>= \case
  []    -> print "Required file path missing"
  (x:_) -> do
    file <- readFile x
    case pProgram (myLexer file) of
      Left err -> do
       putStrLn "SYNTAX ERROR"
       putStrLn err
       exitFailure
      Right prg -> case rename prg of
        Right prg -> do
            putStrLn "RENAME SUCCESSFUL"
            putStrLn $ printTree prg
        Left err -> do
            putStrLn "FAILED RENAMING"
            putStrLn . show $ err
            exitFailure
