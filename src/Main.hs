{-# LANGUAGE LambdaCase #-}

module Main where

import           Codegen.Codegen      (generateCode)
import           GHC.IO.Handle.Text   (hPutStrLn)
import           Grammar.ErrM         (Err)
import           Grammar.Par          (myLexer, pProgram)
import           Grammar.Print        (printTree)

-- import           Interpreter        (interpret)
import           Control.Monad        (when)
import           Data.List.Extra      (isSuffixOf)
import           LambdaLifter         (lambdaLift)
import           Renamer              (rename)
import           System.Directory     (createDirectory, doesPathExist,
                                       getDirectoryContents,
                                       removeDirectoryRecursive,
                                       setCurrentDirectory)
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure, exitSuccess)
import           System.IO            (stderr)
import           System.Process.Extra (spawnCommand, waitForProcess)
import           TypeChecker          (typecheck)

main :: IO ()
main =
    getArgs >>= \case
        [] -> print "Required file path missing"
        ("-d": s : _) -> main' True s
        (s : _) -> main' False s

main' :: Bool -> String -> IO ()
main' debug s = do
    file <- readFile s

    printToErr "-- Parse Tree -- "
    parsed <- fromSyntaxErr . pProgram $ myLexer file
    printToErr $ printTree parsed

    printToErr "\n-- Renamer --"
    let renamed = rename parsed
    printToErr $ printTree renamed

    printToErr "\n-- TypeChecker --"
    typechecked <- fromTypeCheckerErr $ typecheck renamed
    printToErr $ printTree typechecked

    printToErr "\n-- Lambda Lifter --"
    let lifted = lambdaLift typechecked
    printToErr $ printTree lifted

    printToErr "\n -- Printing compiler output to stdout --"
    compiled <- fromCompilerErr $ generateCode lifted
    --putStrLn compiled

    check <- doesPathExist "output"
    when check (removeDirectoryRecursive "output")
    createDirectory "output"
    writeFile "output/llvm.ll" compiled
    if debug then debugDotViz else putStrLn compiled


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
    where
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

fromInterpreterErr :: Err a -> IO a
fromInterpreterErr =
    either
        ( \err -> do
            putStrLn "\nINTERPRETER ERROR"
            putStrLn err
            exitFailure
        )
        pure
