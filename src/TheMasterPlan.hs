module TheMasterPlan where
{-| I'm trying something a little weird here. This file models each step in
the build process, so you will see a sequence of types representing "all the
data we have so far" formatted in a way that will be nice for the next stage.

The idea is that our implementation should be guiding us between these models.
-}

import Control.Applicative ((<$>), (<*>))
import Data.Binary
import qualified Data.Map as Map
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg


-- UNIQUE IDENTIFIERS FOR MODULES

data CanonicalModule = CanonicalModule
    { package :: Package
    , name :: Module.Name
    }
    deriving (Eq, Ord)


type Package = (Pkg.Name, Pkg.Version)


-- CRAWL AN INDIVIDUAL PACKGE

type ModuleGraph = Map.Map CanonicalModule ModuleInfo


data ModuleInfo
    = Elm Location [CanonicalModule]
    | JS Location


data Location = Location
    { _relativePath :: FilePath
    , _package :: Package
    }


-- BUILD-FRIENDLY SUMMARY

{-| Combines the ModuleGraph with all cached build information. At this
stage we crawl any cached interface files. File changes may have invalidated
these cached interfaces, so we filter out any stale interfaces.

The resulting format is very convenient for managing parallel builds.
-}
data BuildGraph =
  BuildGraph
    { blockedModules :: Map.Map CanonicalModule BuildInfo
    , completedInterfaces :: Map.Map CanonicalModule Module.Interface
    }


{-| Everything you need to know to build a file.

  * blocking - modules I depend upon that are not ready yet
  * location - location of source code for when its time to compile

We remove modules from 'blocking' as the build progresses and interfaces are
produced. When 'blocking' is empty, it is safe to add this module to the build
queue.
-}
data BuildInfo = BuildInfo
    { blocking :: [CanonicalModule]
    , location :: Location
    }


-- BINARY

instance Binary CanonicalModule where
  get =
    CanonicalModule <$> get <*> get

  put (CanonicalModule pkg nm) =
    do  put pkg
        put nm

{--
instance Binary ModuleGraph where
  get =
    ModuleGraph <$> get <*> get

  put (ModuleGraph elms jss) =
    do  put elms
        put jss


instance Binary ModuleInfo where
  get =
    ModuleInfo <$> get <*> get

  put (ModuleInfo locations dependencies) =
    put locations >> put dependencies


instance Binary Location where
  get =
    Location <$> get <*> get

  put (Location relative pkg) =
    do  put relative
        put pkg
--}