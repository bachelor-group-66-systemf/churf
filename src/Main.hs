{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import           Control.Monad               (when)
import           Data.Bool                   (bool)
import           Data.List.Extra             (isSuffixOf)
import           Data.Maybe                  (fromJust, isNothing)
import           GHC.IO.Handle.Text          (hPutStrLn)
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

import           Codegen.Codegen             (generateCode)
import           Compiler                    (compile)
import           Grammar.ErrM                (Err)
import           Grammar.Par                 (myLexer, pProgram)
import           Grammar.Print               (printTree)
import           LambdaLifter                (lambdaLift)
import           Monomorphizer.Monomorphizer (monomorphize)
import           Renamer.Renamer             (rename)
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
    header = "Usage: language [--help] [-d|--debug] [-t|type-checker bi/hm] FILE \n"

flags :: [OptDescr (Options -> Options)]
flags =
    [ Option ['d'] ["debug"] (NoArg enableDebug) "Print debug messages."
    , Option ['t'] ["type-checker"] (ReqArg chooseTypechecker "bi/hm") "Choose type checker. Possible options are bi and hm"
    , Option [] ["help"] (NoArg enableHelp) "Print this help message"
    ]

initOpts :: Options
initOpts =
    Options
        { help = False
        , debug = False
        , typechecker = Nothing
        }

enableHelp :: Options -> Options
enableHelp opts = opts{help = True}

enableDebug :: Options -> Options
enableDebug opts = opts{debug = True}

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
    , typechecker :: Maybe TypeChecker
    }

main' :: Options -> String -> IO ()
main' opts s = do
    file <- readFile s

    printToErr "-- Parse Tree -- "
    parsed <- fromSyntaxErr . pProgram $ myLexer file
    bool (printToErr $ printTree parsed) (printToErr $ show parsed) opts.debug

    printToErr "\n-- Renamer --"
    renamed <- fromRenamerErr . rename $ parsed
    bool (printToErr $ printTree renamed) (printToErr $ show renamed) opts.debug

    printToErr "\n-- TypeChecker --"
    typechecked <- fromTypeCheckerErr $ typecheck (fromJust opts.typechecker) renamed
    bool (printToErr $ printTree typechecked) (printToErr $ show typechecked) opts.debug

    printToErr "\n-- Lambda Lifter --"
    --let lifted = lambdaLift typechecked
    --printToErr $ printTree lifted

    -- printToErr "\n-- Lambda Lifter --"
    -- let lifted = lambdaLift typechecked
    -- printToErr $ printTree lifted
    --
    printToErr "\n -- Compiler --"
    generatedCode <- fromCompilerErr $ generateCode (monomorphize typechecked)
    -- putStrLn generatedCode

    check <- doesPathExist "output"
    when check (removeDirectoryRecursive "output")
    createDirectory "output"
    when opts.debug $ do
        _ <- writeFile "output/llvm.ll" generatedCode
        debugDotViz

    compile generatedCode
    spawnWait "./output/hello_world"
    -- interpred <- fromInterpreterErr $ interpret lifted
    -- putStrLn "\n-- interpret"
    -- print interpred

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

fromCompilerErr :: Err a -> IO a
fromCompilerErr =
    either
        ( \err -> do
            putStrLn "\nCOMPILER ERROR"
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

fromRenamerErr :: Err a -> IO a
fromRenamerErr =
    either
        ( \err -> do
            putStrLn "\nRENAMER ERROR"
            putStrLn err
            exitFailure
        )
        pure

fromInterpreterErr :: Err a -> IO a
fromInterpreterErr =
    either
        ( \err -> do
            putStrLn "\nINTERPRETER ERROR"
            putStrLn err
            exitFailure
        )
        pure
