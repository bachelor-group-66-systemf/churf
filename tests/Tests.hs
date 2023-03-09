{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Main where
import Grammar.Abs  (Ident (Ident), Literal (LInt))
import qualified TypeChecker.TypeCheckerIr as T

import Monomorpher.Monomorpher (monomorphize)
import Grammar.Print (printTree)
import           System.IO                 (stderr)
import           GHC.IO.Handle.Text        (hPutStrLn)


printToErr :: String -> IO ()
printToErr = hPutStrLn stderr

main :: IO ()
main = do
  -- Only demonstrations for now, will fail if error is thrown.
  simpleDemo

-- A simple demo
simpleDemo = do
  demo "main = f 5" $ simpleProgram [f] "f" 5
  demo "main = p 5" $ simpleProgram [p] "p" 5
  demo "main = g 5" $ simpleProgram [g, p] "g" 5

-- Nice demo 👍
demo :: String -> T.Program -> IO ()
demo label prg = do
  printToErr $ "#### " ++ label ++ " ####"
  printToErr "    * Lifted Tree *    "
  printToErr $ printTree prg
  printToErr "    * Monomorphized Tree *    "
  printToErr $ printTree (monomorphize prg)
  printToErr "##########\n"

-- Programs in the form of "main = 'func' 'x'"
simpleProgram :: [T.Bind] -> T.Ident -> Int -> T.Program
simpleProgram binds fToCall input = T.Program (T.Bind ("main", typeInt) [] (simpleProgramExp fToCall):binds)
simpleProgramExp func = T.EApp typeInt (T.EId (func, typeIntToInt)) (T.ELit typeInt $ LInt 5)

-- f :: Int -> Int
-- f x = x + x
f = T.Bind ("f", typeIntToInt) [("x", typeInt)] fExp
fExp = T.EAdd typeInt (T.EId ("x", typeInt)) (T.EId (Ident "x", typeInt))

-- p :: a -> a
-- p x = x + x
p = T.Bind (Ident "p", typeAToA) [(Ident "x", typeA)] pExp
pExp = T.EAdd typeA (T.EId (Ident "x", typeA)) (T.EId (Ident "x", typeA))

-- g :: a -> a
-- g x = x + (p x)
g = T.Bind (Ident "g", typeAToA) [("x", typeA)] gExp
gExp = T.EAdd typeA (T.EId (Ident "x", typeA)) (T.EApp typeA (T.EId (Ident "p", typeAToA)) (T.EId (Ident "x", typeA)))

-- | Reusable test constructs for Monomorpher.
typeInt = T.TMono $ Ident "Int"

typeIntToInt = T.TArr typeInt typeInt

typeA = T.TPol $ Ident "a"

typeAToA = T.TArr typeA typeA

