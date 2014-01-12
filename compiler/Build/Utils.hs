module Build.Utils where

import System.FilePath ((</>), replaceExtension)
import qualified Build.Flags as Flag

buildPath :: Flag.Flags -> FilePath -> String -> FilePath
buildPath flags filePath ext =
    Flag.build_dir flags </> replaceExtension filePath ext

cachePath :: Flag.Flags -> FilePath -> String -> FilePath
cachePath flags filePath ext =
    Flag.cache_dir flags </> replaceExtension filePath ext

noelmo :: Flag.Flags -> FilePath -> FilePath
noelmo flags filePath =
    cachePath flags filePath "noelmo"

noelmi :: Flag.Flags -> FilePath -> FilePath
noelmi flags filePath =
    cachePath flags filePath "noelmi"
