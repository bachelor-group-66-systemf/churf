{-# LANGUAGE LambdaCase #-}
module Main where

import           Grammar.ErrM       (Err)
import           Grammar.Par        (myLexer, pProgram)
import           Grammar.Print      (printTree)
import           Interpreter        (interpret)
import           LambdaLifter       (abstract, freeVars, lambdaLift)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)

main :: IO ()
main = getArgs >>= \case
  []    -> print "Required file path missing"
  (s:_) -> main' s

main' :: String -> IO ()
main' s = do
  file   <- readFile s

  putStrLn "\n-- parse"
  parsed    <- fromSyntaxErr . pProgram $ myLexer file
  putStrLn $ printTree parsed

  putStrLn "\n-- Lambda Lifter"
  let lifted = lambdaLift parsed
  putStrLn $ printTree lifted

  interpred <- fromInterpreterErr $ interpret lifted
  putStrLn "\n-- interpret"
  print interpred

  exitSuccess


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


