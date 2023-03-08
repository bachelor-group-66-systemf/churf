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

-- A simple demo
simpleDemo = do
  printToErr "#### f"
  printToErr "-- Lifted Tree --"
  printToErr $ printTree example1
  printToErr "-- Monomorphized Tree --"
  printToErr $ printTree (monomorphize example1)

  printToErr "#### p"
  printToErr "-- Lifted Tree --"
  printToErr $ printTree example2
  printToErr "-- Monomorphized Tree --"
  printToErr $ printTree (monomorphize example2)

  printToErr "#### g"
  printToErr "-- Lifted Tree --"
  printToErr $ printTree example3
  printToErr "-- Monomorphized Tree --"
  printToErr $ printTree (monomorphize example3)

main :: IO ()
main = do
  return ()

-- | Reusable test constructs for Monomorpher.
typeInt = T.TMono $ Ident "Int"

typeIntToInt = T.TArr typeInt typeInt

typeA = T.TPol $ Ident "a"

typeAToA = T.TArr typeA typeA

-- f :: Int -> Int
-- f x = x + x
fName = (Ident "f", typeIntToInt)
fArg1 = (Ident "x", typeInt)
fArgs = [fArg1]
fExp = T.EAdd typeInt (T.EId (Ident "x", typeInt)) (T.EId (Ident "x", typeInt))
f = T.Bind fName fArgs fExp

-- p :: a -> a
-- p x = x + x
pName = (Ident "p", typeAToA)
pArg1 = (Ident "x", typeA)
pArgs = [pArg1]
pExp = T.EAdd typeA (T.EId (Ident "x", typeA)) (T.EId (Ident "x", typeA))
p = T.Bind pName pArgs pExp

-- g :: a -> a
-- g x = x + (p x)
gName = (Ident "g", typeAToA)
gArg1 = (Ident "x", typeA)
gArgs = [gArg1]
gExp = T.EAdd typeA (T.EId (Ident "x", typeA)) (T.EApp typeA (T.EId (Ident "p", typeAToA)) (T.EId (Ident "x", typeA)))
g = T.Bind gName gArgs gExp

-- | Examples
mainName = (Ident "main", typeInt)
-- func 5
mainBoilerProg func binds = T.Program (T.Bind mainName [] (mainBoilerExp func) : binds)
mainBoilerExp func = T.EApp typeInt (T.EId (Ident func, typeIntToInt)) (T.ELit typeInt $ LInt 5)

-- main = f 5
example1 = mainBoilerProg "f" [f]

-- main = p 5
example2 = mainBoilerProg "p" [p]

-- main = g 5
example3 = mainBoilerProg "g" [g, p]

