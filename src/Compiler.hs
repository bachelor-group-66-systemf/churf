module Compiler (compile) where

import System.Process.Extra (
    readCreateProcess,
    shell,
 )

-- spawnWait s = spawnCommand s >>= \s >>= waitForProcess

optimize :: String -> IO String
optimize = readCreateProcess (shell "opt --O3 -S")

compileClang :: String -> IO String
compileClang =
    readCreateProcess . shell $
        unwords
            [ "clang++" -- , "-Lsrc/GC/lib/", "-l:libgcoll.a"
            , "-fno-exceptions -x"
            , "ir"
            , "-o"
            , "output/hello_world"
            , "-"
            ]

compile :: String -> IO String
compile s = optimize s >>= compileClang
