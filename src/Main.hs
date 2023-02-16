{-# LANGUAGE LambdaCase #-}
module Main where

import           Compiler           (compile)
import           GHC.IO.Handle.Text (hPutStrLn)
import           Grammar.ErrM       (Err)
import           Grammar.Par        (myLexer, pProgram)
import           Grammar.Print      (printTree)
--import           Interpreter        (interpret)
import           LambdaLifter       (lambdaLift)
import           Renamer            (rename)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr)
import           TypeChecker        (typecheck)

main :: IO ()
main = getArgs >>= \case
  []    -> print "Required file path missing"
  (s:_) -> main' s

main' :: String -> IO ()
main' s = do
  file   <- readFile s

  printToErr "-- Parse Tree -- "
  parsed    <- fromSyntaxErr . pProgram $ myLexer file
  printToErr $ printTree parsed

  putStrLn "\n-- Renamer --"
  let renamed = rename parsed
  putStrLn $ printTree renamed

  putStrLn "\n-- TypeChecker --"
  typechecked <- fromTypeCheckerErr $ typecheck renamed
  putStrLn $ printTree typechecked

  printToErr "\n-- Lambda Lifter --"
  let lifted = lambdaLift typechecked
  printToErr $ printTree lifted

  printToErr "\n -- Printing compiler output to stdout --"
  compiled  <- fromCompilerErr $ compile lifted
  putStrLn compiled
  writeFile "llvm.ll" compiled

  -- interpred <- fromInterpreterErr $ interpret lifted
  -- putStrLn "\n-- interpret"
  -- print interpred

  exitSuccess

printToErr :: String -> IO ()
printToErr = hPutStrLn stderr

fromCompilerErr :: Err a -> IO a
fromCompilerErr = either
  (\err -> do
    putStrLn "\nCOMPILER ERROR"
    putStrLn err
    exitFailure)
 pure

fromSyntaxErr :: Err a -> IO a
fromSyntaxErr = either
  (\err -> do
    putStrLn "\nSYNTAX ERROR"
    putStrLn err
    exitFailure)
 pure

fromTypeCheckerErr :: Err a -> IO a
fromTypeCheckerErr = either
  (\err -> do
    putStrLn "\nTYPECHECKER ERROR"
    putStrLn err
    exitFailure)
 pure

fromInterpreterErr :: Err a -> IO a
fromInterpreterErr = either
  (\err -> do
    putStrLn "\nINTERPRETER ERROR"
    putStrLn err
    exitFailure)
 pure


