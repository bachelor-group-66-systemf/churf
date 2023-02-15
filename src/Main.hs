{-# LANGUAGE LambdaCase #-}
module Main where

import           Grammar.ErrM       (Err)
import           Grammar.Par        (myLexer, pProgram)
import           Grammar.Print      (printTree)
--import           Interpreter        (interpret)
import           LambdaLifter       (abstract, freeVars, lambdaLift)
import           Renamer            (rename)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           TypeChecker        (typecheck)

main :: IO ()
main = getArgs >>= \case
  []    -> print "Required file path missing"
  (s:_) -> main' s

main' :: String -> IO ()
main' s = do
  file   <- readFile s

  putStrLn "\n-- Parser"
  parsed    <- fromSyntaxErr . pProgram $ myLexer file
  putStrLn $ printTree parsed

  putStrLn "\n-- Renamer"
  let renamed = rename parsed
  putStrLn $ printTree renamed

  putStrLn "\n-- TypeChecker"
  typechecked <- fromTypeCheckerErr $ typecheck renamed
  putStrLn $ printTree typechecked

  putStrLn "\n-- Lambda Lifter"
  let lifted = lambdaLift typechecked
  putStrLn $ printTree lifted

  -- interpred <- fromInterpreterErr $ interpret lifted
  -- putStrLn "\n-- interpret"
  -- print interpred

  exitSuccess


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


