{-# LANGUAGE LambdaCase #-}

module Main where

import           Codegen.Codegen           (compile)
import           GHC.IO.Handle.Text        (hPutStrLn)
import           Grammar.ErrM              (Err)
import           Grammar.Par               (myLexer, pProgram)
import           Grammar.Print             (printTree)

import           LambdaLifter.LambdaLifter (lambdaLift)
import           Renamer.Renamer           (rename)
import           System.Environment        (getArgs)
import           System.Exit               (exitFailure, exitSuccess)
import           System.IO                 (stderr)
import           TypeChecker.TypeChecker   (typecheck)
import Monomorpher.Monomorpher (monomorphize)

main :: IO ()
main =
    getArgs >>= \case
        [] -> print "Required file path missing"
        (s : _) -> main' s

main' :: String -> IO ()
main' s = do
    file <- readFile s

    printToErr "-- Parse Tree -- "
    parsed <- fromSyntaxErr . pProgram $ myLexer file
    printToErr $ printTree parsed

    printToErr "\n-- Renamer --"
    let renamed = rename parsed
    printToErr $ printTree renamed

    printToErr "\n-- TypeChecker --"
    typechecked <- fromTypeCheckerErr $ typecheck renamed
    printToErr $ printTree typechecked

    --printToErr "\n-- TreeConverter --"
    --converted <- fromTypeCheckerErr $ convertToTypecheckerIR renamed
    --printToErr $ printTree converted

    printToErr "\n-- Lambda Lifter --"
    let lifted = lambdaLift typechecked
    printToErr $ printTree lifted

    printToErr "\n -- Monomorphizer --"
    let monomorphed = monomorphize lifted
    printToErr $ printTree monomorphed

    --printToErr "\n -- Printing compiler output to stdout --"
    --compiled <- fromCompilerErr $ compile lifted
    --putStrLn compiled
    --writeFile "llvm.ll" compiled

    exitSuccess

printToErr :: String -> IO ()
printToErr = hPutStrLn stderr

fromCompilerErr :: Err a -> IO a
fromCompilerErr =
    either
        ( \err -> do
            putStrLn "\nCOMPILER ERROR"
            putStrLn err
            exitFailure
        )
        pure

fromSyntaxErr :: Err a -> IO a
fromSyntaxErr =
    either
        ( \err -> do
            putStrLn "\nSYNTAX ERROR"
            putStrLn err
            exitFailure
        )
        pure

fromTypeCheckerErr :: Err a -> IO a
fromTypeCheckerErr =
    either
        ( \err -> do
            putStrLn "\nTYPECHECKER ERROR"
            putStrLn err
            exitFailure
        )
        pure
