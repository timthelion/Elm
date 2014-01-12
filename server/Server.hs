module Main where

import Control.Monad (msum,guard,when)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.List (isPrefixOf, isSuffixOf, (\\))
import Data.Version (showVersion)
import Happstack.Server
import Happstack.Server.Compression
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import GHC.IO.Handle
import qualified Noelm.Internal.Paths as NoelmPaths
import Paths_noelm_server

runtime = "/noelm-runtime.js"

serve :: Int -> String -> IO ()
serve portNumber libLoc = do
  putStrLn $ "Noelm Server " ++ showVersion version ++
             ": running at <http://localhost:" ++ show portNumber ++ ">"
  putStrLn "Just refresh a page to recompile it!"
  simpleHTTP httpConf $ do
         _ <- compressedResponseFilter
         msum [ uriRest serveNoelm
              , uriRest (serveLib libLoc)
              , serveDirectory EnableBrowsing [] "."
              ]
  where httpConf = nullConf { port = portNumber }

pageTitle :: String -> String
pageTitle = dropExtension . takeBaseName

serveNoelm :: FilePath -> ServerPartT IO Response
serveNoelm fp =
  do fileExists <- liftIO $ doesFileExist file
     guard (fileExists && takeExtension fp == ".noelm")
     onSuccess compile serve
  where
    file = tail fp

    compile = liftIO $ createProcess $ (proc "noelm" args) { std_out = CreatePipe }
        where args = [ "--make", "--runtime=" ++ runtime, file ]

    serve = serveFile (asContentType "text/html")
                      ("build" </> replaceExtension file "html")

onSuccess action success = do
  (_, stdout, _, handle) <- action
  exitCode <- liftIO $ waitForProcess handle
  case (exitCode, stdout) of
    (ExitFailure 127, _) ->
        badRequest $ toResponse "Error: noelm binary not found in your path."
    (ExitFailure _, Just out) ->
        do str <- liftIO $ hGetContents out
           badRequest $ toResponse str
    (ExitFailure _, Nothing) ->
        badRequest $ toResponse "See command line for error message."
    (ExitSuccess, _) -> success


serveLib :: FilePath -> String -> ServerPartT IO Response
serveLib libLoc fp = do
  guard (fp == runtime)
  serveFile (asContentType "application/javascript") libLoc

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ("--help":_) = putStrLn usage
parse ("--version":_) = putStrLn ("The Noelm Server " ++ showVersion version)
parse args =
  if null remainingArgs then
      serve portNumber elmRuntime
  else
      putStrLn usageMini

  where
    runtimeArg = filter (isPrefixOf "--runtime-location=") args
    portArg = filter (isPrefixOf "--port=") args
    remainingArgs = (args \\ runtimeArg) \\ portArg

    argValue arg = tail $ dropWhile (/= '=') (head arg)
    portNumber = if null portArg then 8000 else read (argValue portArg) :: Int
    elmRuntime = if null runtimeArg then
                     ElmPaths.runtime
                 else
                     argValue runtimeArg

usageMini :: String
usageMini =
  "Usage: noelm-server [OPTIONS]\n\
  \Try `noelm-server --help' for more information."

usage :: String
usage =
  "Usage: noelm-server [OPTIONS]\n\
  \Compiles and serves .noelm files from the current directory.\n\
  \Example: noelm-server\n\
  \\n\
  \Server configuration:\n\
  \  --port               set the port to listen on (default: 8000)\n\
  \\n\
  \Resource Locations:\n\
  \  --runtime-location   set the location of the Noelm runtime\n\
  \\n\
  \Compiler Information:\n\
  \  --version            print the version information and exit\n\
  \  --help               display this help and exit\n\
  \\n\"
