{-# LANGUAGE LambdaCase #-}

module Main where

-- import           Codegen.Codegen           (compile)
import Grammar.ErrM (Err)
import Grammar.Par (myLexer, pProgram)
import Grammar.Print (printTree)

-- import           LambdaLifter.LambdaLifter (lambdaLift)
import Renamer.Renamer (rename)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

-- import TypeChecker.TypeChecker (typecheck)

main :: IO ()
main =
    getArgs >>= \case
        [] -> print "Required file path missing"
        (s : _) -> main' s

main' :: String -> IO ()
main' s = do
    file <- readFile s

    putStrLn "-- Parse Tree -- "
    parsed <- fromSyntaxErr . pProgram $ myLexer file
    putStrLn $ printTree parsed

    putStrLn "\n-- Renamer --"
    renamed <- fromRenamerErr . rename $ parsed
    putStrLn $ printTree renamed

    -- putStrLn "\n-- TypeChecker --"
    -- typechecked <- fromTypeCheckerErr $ typecheck renamed
    -- putStrLn $ show typechecked

    -- putStrLn "\n-- Lambda Lifter --"
    -- let lifted = lambdaLift typechecked
    -- putStrLn $ printTree lifted

    -- putStrLn "\n -- Printing compiler output to stdout --"
    -- compiled <- fromCompilerErr $ compile lifted
    -- putStrLn compiled

    exitSuccess

fromCompilerErr :: Err a -> IO a
fromCompilerErr =
    either
        ( \err -> do
            putStrLn "\nCOMPILER ERROR"
            putStrLn err
            exitFailure
        )
        pure

fromRenamerErr :: Err a -> IO a
fromRenamerErr =
    either
        ( \err -> do
            putStrLn "\nRENAME ERROR"
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
