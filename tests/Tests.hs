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
  demo "main = f 5" $ simpleProgram [f] 
    (mainApp (T.EId ("f", typeIntToInt)) lit5)
  demo "main = bigId 5" $ simpleProgram [bigId] 
    (mainApp (T.EId ("bigId", typeIntToInt)) lit5)
  demo "main = g 5" $ simpleProgram [g, bigId] 
    (mainApp (T.EId ("g", typeIntToInt)) lit5)
  demo "main = (bigConst 5) ((bigConst 5) True)" $ simpleProgram [bigConst]
    (T.EApp typeInt
      -- (bigConst 5)
      (T.EApp typeIntToInt (T.EId ("bigConst", typeIntToIntToInt)) lit5)
      -- ((bigConst 5) True)
      (T.EApp typeInt
        (T.EApp typeBoolToInt
          (T.EId ("bigConst", typeIntToBoolToInt))
          lit5
        )
        litTrue
      ) 
    )

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
simpleProgram :: [T.Bind] -> T.Exp -> T.Program
simpleProgram binds input = T.Program (T.Bind ("main", typeInt) [] input:binds)

-- Applies two expressions, has type Int
mainApp :: T.Exp -> T.Exp -> T.Exp
mainApp = T.EApp typeInt

-- f :: Int -> Int
-- f x = x + x
f = T.Bind ("f", typeIntToInt) [("x", typeInt)] fExp
fExp = T.EAdd typeInt (T.EId ("x", typeInt)) (T.EId ("x", typeInt))

-- bigId :: a -> a
-- bigId x = x
bigId = T.Bind (Ident "bigId", typeAToA) [(Ident "x", typeA)] bigIdExp
bigIdExp = T.EAdd typeA (T.EId (Ident "x", typeA)) (T.EId ("x", typeA))

-- bigConst :: a -> a -> a
-- bigConst x y = x
bigConst = T.Bind ("bigConst", typeAToAToA) [("x", typeA), ("y", typeA)] bigConstExp
bigConstExp = T.EId ("x", typeA)

-- g :: a -> a
-- g x = x + (bigId x)
g = T.Bind ("g", typeAToA) [("x", typeA)] gExp
gExp = T.EAdd typeA (T.EId ("x", typeA)) (T.EApp typeA (T.EId ("bigId", typeAToA)) (T.EId ("x", typeA)))

-- | Reusable test constructs for Monomorpher.
typeInt = T.TMono "Int"

typeIntToInt = T.TArr typeInt typeInt

typeIntToIntToInt = T.TArr typeInt typeIntToInt


typeA = T.TPol $ Ident "a"

typeAToA = T.TArr typeA typeA

typeAToAToA = T.TArr typeA typeAToA


typeBool = T.TMono "Bool"

typeBoolToBool = T.TArr typeBool typeBool

typeBoolToBoolToBool = T.TArr typeBool typeBoolToBool


lit5 = T.ELit typeInt $ T.LInt 5

litTrue = T.ELit typeBool T.LBool


typeBoolToInt = T.TArr typeBool typeInt
typeIntToBoolToInt = T.TArr typeInt typeBoolToInt

