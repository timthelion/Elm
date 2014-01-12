module Metadata.Prelude (interfaces, add) where

import qualified Data.Map as Map
import qualified Control.Exception as E
import qualified Paths_Noelm as Path
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import SourceSyntax.Module
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS

import qualified InterfaceSerialization as IS

add :: Module t v -> Module t v
add (Module name exs ims stmts) = Module name exs (customIms ++ ims) stmts
    where
      customIms = concatMap addModule prelude

      addModule (n, method) = case lookup n ims of
                                Nothing     -> [(n, method)]
                                Just (As m) -> [(n, method)]
                                Just _      -> []

prelude :: [(String, ImportMethod)]
prelude = text ++ map (\n -> (n, Hiding [])) modules
  where
    text = map ((,) "Text") [ As "Text", Hiding ["link", "color", "height"] ]
    modules = [ "Basics", "Signal", "List", "Maybe", "Time", "Prelude"
              , "Graphics.Element", "Color", "Graphics.Collage" ]

interfaces :: IO Interfaces
interfaces = safeReadDocs =<< Path.getDataFileName "interfaces.data"

safeReadDocs :: FilePath -> IO Interfaces
safeReadDocs name =
    E.catch (readDocs name) $ \err -> do
      let _ = err :: IOError
      putStrLn $ unlines [ "Error reading types for standard library!"
                         , "    The file should be at " ++ name
                         , "    Please report an issue at github.com/timthelion/Noelm"
                         , "    and specify your versions of Noelm and your OS" ]
      exitFailure

firstModuleInterface :: [(String, ModuleInterface)] ->
                        Either String (String, ModuleInterface)
firstModuleInterface interfaces =
    case interfaces of
      []      -> Left "No interfaces found in serialized Prelude!"
      iface:_ -> Right iface

readDocs :: FilePath -> IO Interfaces
readDocs filePath = do
  bytes <- IS.loadInterface filePath
  let interfaces = IS.interfaceDecode filePath =<< bytes

      -- Although every ModuleInterface that is deserialized in this collection
      -- contains the compiler version, we only check the first ModuleInterface
      -- since it doesn't make sense that different modules in Prelude would
      -- have been compiled by different compiler versions.
      isValid = IS.validVersion filePath =<< firstModuleInterface =<< interfaces

  case (interfaces, isValid) of
    (_, Left err) -> do
      hPutStrLn stderr err
      exitFailure

    (Right ifaces, _) -> return $ Map.fromList ifaces