{-# LANGUAGE LambdaCase #-}

module Main where

import Grammar.Par (myLexer, pProgram)
-- import           TypeChecker.TypeChecker (typecheck)

import Grammar.Print (printTree)
import Renamer.Renamer (rename)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import TypeChecker.TypeChecker (typecheck)

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
                    case rename prg of
                        Left err -> do
                            putStrLn "FAILED RENAMING"
                            print err
                            exitFailure
                        Right prg -> do
                            putStrLn ""
                            putStrLn " ----- RENAMER ----- "
                            putStrLn ""
                            putStrLn . printTree $ prg
                            case typecheck prg of
                                Left err -> do
                                    putStrLn "TYPECHECK ERROR"
                                    print err
                                    exitFailure
                                Right prg -> do
                                    putStrLn ""
                                    putStrLn " ----- TYPECHECKER ----- "
                                    putStrLn ""
                                    putStrLn . printTree $ prg
                                    exitSuccess
