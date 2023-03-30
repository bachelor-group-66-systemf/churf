module Compiler (compile) where

import System.Process.Extra (
    readCreateProcess,
    shell,
 )

-- spawnWait s = spawnCommand s >>= \s >>= waitForProcess

optimize :: String -> IO String
optimize = readCreateProcess (shell "opt --O3 --tailcallopt -S")

compileClang :: String -> IO String
compileClang =
    readCreateProcess . shell $
        unwords
            [ "clang++" -- , "-Lsrc/GC/lib/", "-l:libgcoll.a"
            , "-fno-rtti"
            , "src/GC/lib/cheap.cpp"
            , "src/GC/lib/event.cpp"
            , "src/GC/lib/heap.cpp"
            , "src/GC/lib/profiler.cpp"
            , "-Wall -Wextra -g -std=gnu++20 -stdlib=libstdc++ -O3"
            , "-Isrc/GC/include"
            , "-x"
            , "ir" -- , "-Lsrc/GC/lib -l:gcoll.a"
            , "-o"
            , "output/hello_world"
            , "-"
            ]

compile :: String -> IO String
compile s = optimize s >>= compileClang
