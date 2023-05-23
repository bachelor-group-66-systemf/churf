{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import           AnnForall                   (annotateForall)
import           Codegen.Codegen             (generateCode)
import           Compiler                    (compile)
import           Control.Monad               (when, (<=<))
import           Data.List.Extra             (isSuffixOf)
import           Data.Maybe                  (fromJust, isNothing)
import           Desugar.Desugar             (desugar)
-- import           Expander                    (expand)
import           GHC.IO.Handle.Text          (hPutStrLn)
import           Grammar.ErrM                (Err)
import           Grammar.Layout              (resolveLayout)
import           Grammar.Par                 (myLexer, pProgram)
import           Grammar.Print               (Print, printTree)
import           LambdaLifter                (lambdaLift)
import           Monomorphizer.Monomorphizer (monomorphize)
import           OrderDefs                   (orderDefs)
import           Renamer.Renamer             (rename)
import           ReportForall                (reportForall)
import           System.Console.GetOpt       (ArgDescr (NoArg, ReqArg),
                                              ArgOrder (RequireOrder),
                                              OptDescr (Option), getOpt,
                                              usageInfo)
import           System.Directory            (createDirectory, doesPathExist,
                                              getDirectoryContents,
                                              removeDirectoryRecursive,
                                              setCurrentDirectory)
import           System.Environment          (getArgs)
import           System.Exit                 (ExitCode (ExitFailure),
                                              exitFailure, exitSuccess,
                                              exitWith)
import           System.IO                   (stderr)
import           System.Process              (spawnCommand, waitForProcess)
import           TypeChecker.TypeChecker     (TypeChecker (Bi, Hm), typecheck)

main :: IO ()
main = getArgs >>= parseArgs >>= uncurry main'

parseArgs :: [String] -> IO (Options, String)
parseArgs argv = case getOpt RequireOrder flags argv of
    (os, f : _, [])
        | opts.help || isNothing opts.typechecker -> do
            hPutStrLn stderr (usageInfo header flags)
            exitSuccess
        | otherwise -> pure (opts, f)
      where
        opts = foldr ($) initOpts os
    (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)
  where
    header = "Usage: language [--help] [-l|--log-intermediate] [-d|--debug] [-m|--disable-gc] [-t|--type-checker bi/hm] [-p|--disable-prelude] <FILE> \n"

flags :: [OptDescr (Options -> Options)]
flags =
    [ Option ['d'] ["debug"] (NoArg $ enableDebug . logIntermediate) "Print debug messages. --debug implies --log-intermediate"
    , Option ['t'] ["type-checker"] (ReqArg chooseTypechecker "bi/hm") "Choose type checker. Possible options are bi and hm"
    , Option ['m'] ["disable-gc"] (NoArg disableGC) "Disables the garbage collector and uses malloc instead."
    , Option ['p'] ["disable-prelude"] (NoArg disablePrelude) "Do not include the prelude"
    , Option ['l'] ["log-intermediate"] (NoArg logIntermediate) "Log intermediate languages"
    , Option [] ["help"] (NoArg enableHelp) "Print this help message"
    ]

initOpts :: Options
initOpts =
    Options
        { help = False
        , debug = False
        , gc = True
        , typechecker = Nothing
        , preludeOpt = False
        , logIL = False
        }

enableHelp :: Options -> Options
enableHelp opts = opts{help = True}

enableDebug :: Options -> Options
enableDebug opts = opts{debug = True}

disableGC :: Options -> Options
disableGC opts = opts{gc = False}

disablePrelude :: Options -> Options
disablePrelude opts = opts{preludeOpt = True}

logIntermediate :: Options -> Options
logIntermediate opts = opts{logIL = True}


chooseTypechecker :: String -> Options -> Options
chooseTypechecker s options = options{typechecker = tc}
  where
    tc = case s of
        "hm" -> pure Hm
        "bi" -> pure Bi
        _    -> Nothing

data Options = Options
    { help        :: Bool
    , debug       :: Bool
    , gc          :: Bool
    , typechecker :: Maybe TypeChecker
    , preludeOpt  :: Bool
    , logIL       :: Bool
    }

main' :: Options -> String -> IO ()
main' opts s =
    let
        log :: (Print a, Show a) => a -> IO ()
        log = printToErr . if opts.debug then show else printTree
     in
        do
            file <- readFile s


            let file' = if opts.preludeOpt then file ++ primitives else file ++ primitives ++ prelude
            parsed <- fromErr . pProgram . resolveLayout True $ myLexer file'
            when opts.logIL (printToErr "-- Parse Tree -- " >> log parsed)


            let desugared = desugar parsed
            when opts.logIL (printToErr "-- Desugar --" >> log desugared)


            _ <- fromErr $ reportForall (fromJust opts.typechecker) desugared
            renamed <- fromErr $ orderDefs <$> (rename <=< annotateForall) desugared
            when opts.logIL (printToErr "\n-- Renamer --" >> log renamed)


            typechecked <- fromErr $ typecheck (fromJust opts.typechecker) renamed
            when opts.logIL (printToErr "\n-- TypeChecker --" >> log typechecked)


            -- let etaexpanded = expand typechecked
            -- when opts.logIL (printToErr "\n-- Eta expander --" >> log etaexpanded)


            let lifted = lambdaLift typechecked
            when opts.logIL (printToErr "\n-- Lambda Lifter --" >> log lifted)


            monomorphized <- fromErr $ monomorphize lifted
            when opts.logIL (printToErr "\n -- Monomorphizer --" >> log monomorphized)


            generatedCode <- fromErr $ generateCode monomorphized (gc opts)
            -- generatedCode <- fromErr $ generateCode monomorphized False

            check <- doesPathExist "output"
            when check (removeDirectoryRecursive "output")
            createDirectory "output"
            createDirectory "output/logs"
            when opts.logIL (writeFile "output/logs/tc.log" (printTree typechecked))
            when opts.debug $ do
                printToErr "\n -- Compiler --"
                writeFile "output/llvm.ll" generatedCode
                --debugDotViz

            compile generatedCode (gc opts)
            -- compile generatedCode False
            printToErr "Compilation done!"
            printToErr "\n-- Program output --"
            print =<< spawnWait "./output/hello_world"

            exitSuccess

debugDotViz :: IO ()
debugDotViz = do
    setCurrentDirectory "output"
    spawnWait "opt -dot-cfg llvm.ll -disable-output"
    content <- filter (isSuffixOf ".dot") <$> getDirectoryContents "."
    let commands = (\p -> "dot " <> p <> " -Tpng -o" <> p <> ".png") <$> content
    mapM_ spawnWait commands
    setCurrentDirectory ".."
    return ()

spawnWait :: String -> IO ExitCode
spawnWait s = spawnCommand s >>= waitForProcess

printToErr :: String -> IO ()
printToErr = hPutStrLn stderr

fromErr :: Err a -> IO a
fromErr = either (\s -> printToErr s >> exitFailure) pure

primitives =
    unlines
        [ ""
        , "data Bool where"
        , "    False : Bool"
        , "    True  : Bool"
        , "\n"
        , ".< : Int -> Int -> Bool"
        , ".< x y = case x of"
        , "    _ => True"
        , "    _ => False"
        , ".- : Int -> Int -> Int"
        , ".- x y = 0"
        , ".+ : Int -> Int -> Int"
        , ".+ x y = 0"
        , ".== : Int -> Int -> Bool"
        , ".== a b = case a < b of"
        , "    False => case b < a of"
        , "        False => True"
        , "        True => False"
        , "    True => False"
        ]

prelude :: String
prelude =
    unlines
        [ "\n"
        , "data Unit where"
        , "  Unit : Unit"
        , "\n"
        , "printChar : Char -> Unit"
        , "printChar = \\x. Unit"
        , "\n"
        , "flipConst : a -> b -> b"
        , "flipConst x y = y"
        , "\n"
        , "const : a -> b -> a"
        , "const x y = x"
        , "\n"
        -- Printing as a list for the demonstration
        , "printStr : List Char -> Unit"
        , "printStr xs = case xs of"
        , "    Nil => Unit"
        , "    Cons x xs => flipConst (printChar x) (printStr xs)"
        , "\n"
        , "printInt : Int -> Unit"
        , "printInt xs = Unit"
        , "\n"
        , "asciiCode : Char -> Int"
        , "asciiCode x = case x of { 'a' => 97; 'b' => 98; 'c' => 99; 'd' => 100; 'e' => 101; 'f' => 102; 'g' => 103; 'h' => 104; 'i' => 105; 'j' => 106; 'k' => 107; 'l' => 108; 'm' => 109; 'n' => 110; 'o' => 111; 'p' => 112; 'q' => 113; 's' => 114; 't' => 115; 'u' => 116; 'v' => 117; 'w' => 118; 'x' => 119; 'y' => 120; 'z' => 121; }"
        , "toChar : Int -> Char"
        , "toChar x = case x of {0 => '0'; 1 => '1'; 2 => '2'; 3 => '3'; 4 => '4'; 5 => '5'; 6 => '6'; 7 => '7'; 8 => '8'; 9 => '9'; }"
        , "\n"
        , "toStr : List Int -> List Char"
        , "toStr xs = case xs of"
        , "    Cons a as => Cons (toChar a) (toStr as)"
        , "    Nil => Nil"
        , "\n"
        , ".++ : List a -> List a -> List a"
        , ".++ as bs = case as of"
        , "    Nil => bs"
        , "    Cons x xs => Cons x (xs ++ bs)"
        , "\n"
        , "data List a where"
        , "  Nil : List a"
        , "  Cons : a -> List a -> List a"
        , "\n"
        , "data Pair a b where"
        , "  Pair : a -> b -> Pair a b"
        , "\n"
        , "printListH : List Int -> Unit"
        , "printListH xs = case xs of"
        , "    Cons a as => flipConst (printInt a) (printListHH as)"
        , "    Nil => Unit"
        , "\n"
        , "printListHH : List Int -> Unit"
        , "printListHH xs = case xs of"
        , "    Nil => Unit"
        , "    Cons a as => flipConst (printChar ',') (flipConst (printInt a) (printListHH as))"
        , "\n"
        , "printList : List Int -> Unit"
        , "printList xs = case Cons (printChar '[') (Cons (printListH xs) (Cons (printChar ']') Nil)) of"
        , "        _ => Unit"
        ]
