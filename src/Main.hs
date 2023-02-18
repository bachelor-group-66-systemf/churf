{-# LANGUAGE LambdaCase #-}

module Main where

import           Grammar.Par        (myLexer, pProgram)
-- import           TypeChecker.TypeChecker (typecheck)

import           Grammar.Print      (printTree)
import           Renamer.RenamerM   (rename)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           TypeChecker.HM     (typecheck)

main :: IO ()
main =
    getArgs >>= \case
        [] -> print "Required file path missing"
        (x : _) -> do
            file <- readFile x
            case pProgram (myLexer file) of
                Left err -> do
                    putStrLn "SYNTAX ERROR"
                    putStrLn err
                    exitFailure
                Right prg -> do
                    putStrLn ""
                    putStrLn " ----- PARSER ----- "
                    putStrLn ""
                    putStrLn . printTree $ prg
                    case typecheck (rename prg) of
                        Left err -> do
                            putStrLn "TYPECHECK ERROR"
                            print err
                            exitFailure
                        Right prg -> do
                            putStrLn ""
                            putStrLn " ----- RAW ----- "
                            putStrLn ""
                            print prg
                            putStrLn ""
                            putStrLn " ----- TYPECHECKER ----- "
                            putStrLn ""
                            putStrLn $ printTree prg
                            exitSuccess
