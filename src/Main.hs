{-# LANGUAGE LambdaCase #-}
module Main where

import           Grammar.Par        (myLexer, pProgram)
import           Grammar.Print      (printTree)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           TypeChecker.TypeChecker (typecheck)
import           Renamer.Renamer (rename)
import           Grammar.Print (prt)

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
        putStrLn ""
        putStrLn " ----- PARSER ----- "
        putStrLn ""
        putStrLn . printTree $ prg
        putStrLn . show $ prg
        case rename prg of
            Left err -> do
              putStrLn "FAILED RENAMING"
              putStrLn . show $ err
              exitFailure
            Right prg ->do
                putStrLn ""
                putStrLn " ----- RENAMER ----- "
                putStrLn ""
                putStrLn . printTree $ prg
                case typecheck prg of
                    Left err -> do
                        putStrLn "TYPECHECK ERROR"
                        putStrLn . show $ err
                        exitFailure
                    Right prg -> do
                        putStrLn ""
                        putStrLn " ----- TYPECHECKER ----- "
                        putStrLn ""
                        putStrLn . printTree $ prg
