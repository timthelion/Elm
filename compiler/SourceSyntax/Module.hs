module SourceSyntax.Module where

import Data.Binary
import Data.List (intercalate)
import qualified Data.Map as Map
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)

import SourceSyntax.Expression (LExpr)
import SourceSyntax.Declaration
import SourceSyntax.Type
import System.FilePath (joinPath)
import Control.Monad (liftM)

import qualified Noelm.Internal.Version as Version

data Module tipe var =
    Module [String] Exports Imports [Declaration tipe var]
    deriving (Show)

type Exports = [String]

type Imports = [(String, ImportMethod)]
data ImportMethod = As String | Importing [String] | Hiding [String]
                    deriving (Eq, Ord, Show)

instance Binary ImportMethod where
    put (As s)          = do put (0 :: Word8)
                             put s

    put (Importing ss) = do put (1 :: Word8)
                            put ss

    put (Hiding ss)    = do put (2 :: Word8)
                            put ss

    get = do tag <- getWord8
             case tag of
               0 -> liftM As        get
               1 -> liftM Importing get
               2 -> liftM Hiding    get
               _ -> error "Error reading valid ImportMethod type from serialized string"

data MetadataModule t v = MetadataModule {
    names     :: [String],
    path      :: FilePath,
    exports   :: [String],
    imports   :: [(String, ImportMethod)],
    program   :: LExpr t v,
    types     :: Map.Map String Type,
    fixities  :: [(Assoc, Int, String)],
    aliases   :: [(String, [String], Type)],
    datatypes :: [ (String, [String], [(String,[Type])]) ],
    foreignImports :: [(String, LExpr t v, String, Type)],
    foreignExports :: [(String, String, Type)]
} deriving Show

type Interfaces = Map.Map String ModuleInterface
type ADT = (String, [String], [(String,[Type])])

data ModuleInterface = ModuleInterface {
    iVersion  :: Version.Version,
    iTypes    :: Map.Map String Type,
    iImports  :: [(String, ImportMethod)],
    iAdts     :: [ADT],
    iAliases  :: [(String, [String], Type)],
    iFixities :: [(Assoc, Int, String)]
} deriving Show

metaToInterface metaModule =
    ModuleInterface
    { iVersion  = Version.noelmVersion
    , iTypes    = types metaModule
    , iImports  = imports metaModule
    , iAdts     = datatypes metaModule
    , iAliases  = aliases metaModule
    , iFixities = fixities metaModule
    }

instance Binary ModuleInterface where
  get = ModuleInterface <$> get <*> get <*> get <*> get <*> get <*> get
  put modul = do
      put (iVersion modul)
      put (iTypes modul)
      put (iImports modul)
      put (iAdts modul)
      put (iAliases modul)
      put (iFixities modul)


instance Binary Assoc where
    get = do n <- getWord8
             return $ case n of
                0 -> L
                1 -> N
                2 -> R
                _ -> error "Error reading valid associativity from serialized string"

    put assoc = putWord8 $ case assoc of { L -> 0 ; N -> 1 ; R -> 2 }
