module Compiler (compile) where

import           Grammar.ErrM         (Err)
import           System.Exit          (exitFailure, exitSuccess)
import           System.IO            (BufferMode (NoBuffering), hClose, hFlush,
                                       hGetContents, hPutStr, hPutStrLn,
                                       hSetBuffering, stderr)
import           System.Process.Extra (CreateProcess (..),
                                       StdStream (CreatePipe), createProcess,
                                       proc, readCreateProcess, shell,
                                       spawnCommand, waitForProcess)

--spawnWait s = spawnCommand s >>= \s >>= waitForProcess

optimize :: String -> IO String
optimize = readCreateProcess (shell "opt --O3 -S")

compileClang :: String -> IO String
compileClang = readCreateProcess . shell
    $ unwords ["clang++"--, "-Lsrc/GC/lib/", "-l:libgcoll.a"
              , "-fno-exceptions -x", "ir" ,"-o" ,"output/hello_world"
              , "-"]

compile :: String -> IO String
compile s = optimize s >>= compileClang
