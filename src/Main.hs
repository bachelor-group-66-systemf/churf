{-# LANGUAGE LambdaCase #-}
module Main where

import           Grammar.Par        (myLexer, pProgram)
import           Grammar.Print      (printTree)
import           LambdaLifter       (abstract, freeVars, lambdaLift)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)

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
      Right prg -> do
        putStrLn "-- Parse"
        putStrLn $ printTree prg
        putStrLn "\n-- Lambda lifter"
        putStrLn . printTree $ lambdaLift prg
        putStrLn ""
        exitSuccess



