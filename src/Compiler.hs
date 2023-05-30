module Compiler (compile) where

import           System.Process.Extra (readCreateProcess, shell)

-- spawnWait s = spawnCommand s >>= \s >>= waitForProcess

optimize :: String -> IO String
optimize = readCreateProcess (shell "opt --O3 --tailcallopt -S")

compileClang :: String -> Bool -> String -> IO String
compileClang name False =
    readCreateProcess . shell $
        unwords
            [ "clang++" -- , "-Lsrc/GC/lib/", "-l:libgcoll.a"
            , "-fno-rtti"
            , "-x"
            , "ir" -- , "-Lsrc/GC/lib -l:gcoll.a"
            , "-o"
            , "output/" <> name
            , "-"
            ]
compileClang name True =
    readCreateProcess . shell $
        unwords
            [ "clang++" -- , "-Lsrc/GC/lib/", "-l:libgcoll.a"
            , "-fno-rtti"
            , "src/GC/lib/cheap.cpp"
            , "src/GC/lib/event.cpp"
            , "src/GC/lib/heap.cpp"
            , "src/GC/lib/profiler.cpp"
            -- , "-Wall -Wextra -g -std=gnu++20 -stdlib=libstdc++"
            , "-w -g -std=gnu++20 -stdlib=libstdc++"
            , "-O3"
            --, "-tailcallopt"
            , "-Isrc/GC/include"
            , "-x"
            , "ir" -- , "-Lsrc/GC/lib -l:gcoll.a"
            , "-o"
            , "output/" <> name
            , "-"
            ]

compile :: String -> String -> Bool -> IO String
compile name s addGc = optimize s >>= compileClang name addGc
