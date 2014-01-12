module Noelm.Internal.Paths where

import System.IO.Unsafe
import qualified Paths_Noelm as This

-- |Name of directory for all of a project's dependencies.
dependencyDirectory :: FilePath
dependencyDirectory = "noelm_dependencies"

-- |Name of the dependency file, specifying dependencies and
--  other metadata for building and sharing projects.
dependencyFile :: FilePath
dependencyFile = "noelm_dependencies.json"

{-# NOINLINE runtime #-}
-- |The absolute path to Noelm's runtime system.
runtime :: FilePath
runtime = unsafePerformIO $ This.getDataFileName "noelm-runtime.js"

{-# NOINLINE docs #-}
-- |The absolute path to Noelm's core library documentation.
docs :: FilePath
docs = unsafePerformIO $ This.getDataFileName "docs.json"
