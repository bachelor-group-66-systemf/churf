{-# LANGUAGE LambdaCase #-}
module Main where

import           Compiler.Compiler  (compile)
import           GHC.IO.Handle.Text (hPutStrLn)
import           Grammar.ErrM       (Err)
import           Grammar.Par        (myLexer, pProgram)
import           Grammar.Print      (printTree)
import           Interpreter        (interpret)
import           LambdaLifter       (abstract, freeVars, lambdaLift)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr)

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

  printToErr "\n-- Lambda Lifter --"
  let lifted = lambdaLift parsed
  printToErr $ printTree lifted

  --putStrLn "\n-- Compiler"
  printToErr "\n -- Printing compiler output to stdout --"
  compiled  <- fromCompilerErr $ compile lifted
  putStrLn compiled

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

fromInterpreterErr :: Err a -> IO a
fromInterpreterErr = either
  (\err -> do
    putStrLn "\nINTERPRETER ERROR"
    putStrLn err
    exitFailure)
 pure


