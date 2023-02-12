module Main where
import           Compiler.Compiler                 (compile)
import           Control.Exception                 (IOException, catch,
                                                    evaluate)
import           Data.List                         (isSuffixOf, sort)
import           Data.List.Extra                   (trim)
import           GHC.IO.Handle                     (BufferMode (NoBuffering),
                                                    hSetBuffering)
import           Grammar.ErrM                      (Err)
import           Grammar.Par                       (myLexer, pProgram)
import           LambdaLifter                      (lambdaLift)
import           System.Directory                  (getDirectoryContents)
import           System.Directory.Internal.Prelude (exitFailure)
import           System.IO                         (hPrint)
import           System.IO.Extra                   (stderr)
import           System.Process                    (CreateProcess (std_in),
                                                    StdStream (CreatePipe),
                                                    createProcess, proc,
                                                    readCreateProcess, shell,
                                                    waitForProcess)
import           Test.Hspec                        (hspec, it, shouldSatisfy)
import           Text.Printf                       (hPrintf)


path :: String
path = "sample-programs/sample-programs"

main :: IO ()
main = do
    dir <- getDirectoryContents path
    -- this is not a good way to grab tests
    -- ideally one would grab all .sf files, remove the sf extension
    -- and and res instead. Going to fix that soon
    let tests = sort $ filter (".sf" `isSuffixOf`) dir
    let results = sort $ filter (".res" `isSuffixOf`) dir
    let combined = zip tests results
    mapM_ (uncurry test) combined

fromErr :: Err a -> IO a
fromErr (Left a) = do
    hPrint stderr a
    exitFailure
fromErr (Right a) = pure a

comp :: String -> IO String
comp file = do
    parsed    <- fromErr . pProgram $ myLexer file
    let lifted = lambdaLift parsed
    fromErr $ compile lifted

test :: String -> String -> IO ()
test t r = do
    test <- readFile (path <> "/" <> t)
    expectedRes <- trim <$> readFile (path <> "/" <> r)
    hspec $ it t $ do
        compiled <- catch (comp test)
                          (const $ return "term" :: IOException -> IO String )
        ev <- catch (trim <$> readCreateProcess (shell "lli") compiled)
                    (\e -> return $ show (e :: IOException))
        ev `shouldSatisfy` (expectedRes ==)
