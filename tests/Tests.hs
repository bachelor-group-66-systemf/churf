{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Main where
import Grammar.Abs  (Ident (Ident), Literal (LInt))
import qualified TypeChecker.TypeCheckerIr as T

import Monomorpher.Monomorpher (monomorphize)
import Grammar.Print (printTree)
import           System.IO                 (stderr)
import           GHC.IO.Handle.Text        (hPutStrLn)
import Test.Hspec

printToErr :: String -> IO ()
printToErr = hPutStrLn stderr

-- A simple demo
simpleDemo = do
  printToErr "# Monomorphic function f"
  printToErr "-- Lifted Tree --"
  printToErr $ printTree example1
  printToErr "-- Monomorphized Tree --"
  printToErr $ printTree (monomorphize example1)

  printToErr "# Polymorphic function p"
  printToErr "-- Lifted Tree --"
  printToErr $ printTree example2
  printToErr "-- Monomorphized Tree --"
  printToErr $ printTree (monomorphize example2)

main :: IO ()
main = do
  return ()

-- | Reusable test constructs for Monomorpher.
typeInt :: T.Type
typeInt = T.TMono $ Ident "Int"

typeIntToInt :: T.Type
typeIntToInt = T.TArr typeInt typeInt

typeA :: T.Type
typeA = T.TPol $ Ident "a"

typeAToA :: T.Type
typeAToA = T.TArr typeA typeA

-- f :: Int -> Int
-- f x = x + x
fName = (Ident "f", typeIntToInt)
fArg1 = (Ident "x", typeInt)
fArgs = [fArg1]
fExp :: T.Exp
fExp = T.EAdd typeInt (T.EId (Ident "x", typeInt)) (T.EId (Ident "x", typeInt))
f :: T.Bind
f = T.Bind fName fArgs fExp

-- f :: a -> a
-- f x = x + x
pName = (Ident "p", typeAToA)
pArg1 = (Ident "x", typeA)
pArgs = [pArg1]
pExp :: T.Exp
pExp = T.EAdd typeA (T.EId (Ident "x", typeA)) (T.EId (Ident "x", typeA))
p :: T.Bind
p = T.Bind pName pArgs pExp


-- | Examples

-- main = f 5
example1Name = (Ident "main", typeInt)
example1Exp :: T.Exp
example1Exp = T.EApp typeInt (T.EId (Ident "f", typeIntToInt)) (T.ELit typeInt $ LInt 5)
example1 :: T.Program
example1 = T.Program [T.Bind example1Name [] example1Exp, f]

-- main = p 5
example2Name = (Ident "main", typeInt)
example2Exp :: T.Exp
example2Exp = T.EApp typeInt (T.EId (Ident "p", typeIntToInt)) (T.ELit typeInt $ LInt 5)
example2 :: T.Program
example2 = T.Program [T.Bind example2Name [] example2Exp, p]

