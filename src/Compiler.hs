module Compiler where

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
optimize prg = do
    result <- readCreateProcess (shell "opt --O3") prg
    putStrLn result


    -- (Just hin, Just hout, _, _) <- createProcess (proc "opt" ["--O3"]){ std_in = CreatePipe, std_out = CreatePipe }
    -- hSetBuffering hin NoBuffering
    -- hPutStrLn hin prg
    -- hFlush hin
    --bytes <- hGetContents hout
    --putStrLn bytes
    pure ""
