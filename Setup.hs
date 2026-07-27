{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
import Data.Either

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Types.BuildInfo
import Distribution.Types.PackageDescription
import Distribution.Types.CondTree
import Distribution.Types.ForeignLib
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Path
import System.Process
import Data.List (intercalate, nub)

import Distribution.Compat.Lens
import qualified Distribution.Types.BuildInfo.Lens as L

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { confHook = patchedConfHook }

patchedConfHook
  :: (GenericPackageDescription, HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
patchedConfHook (gpd, hbi) flags
  | Just True <- lookupFlagAssignment ("python3-config") $ configConfigurationsFlags flags
  = do cflags_raw  <- readProcess "python3-config" ["--cflags"] ""
       ldflags_raw <- readProcess "python3-config" ["--embed", "--ldflags"] ""  
       -- Split flags
       let (inc_dirs,cflags) = partitionEithers
             [ case flag of
                 '-':'I':path -> Left  path
                 _            -> Right flag
             | flag <- tokenizeArguments cflags_raw
             ]
           (ldflags, (libs, lib_dirs)) = fmap partitionEithers $ partitionEithers
             [ case flag of
                 '-':'l':path -> Right (Left  path)
                 '-':'L':path -> Right (Right path)
                 _            -> Left flag
             | flag <- tokenizeArguments ldflags_raw
             ]
       let tweakLib lib = case lib.libName of
             LMainLibName -> lib & L.ccOptions    %~ (++ cflags)
                                 & L.includeDirs  %~ (++ inc_dirs)
                                 & L.ldOptions    %~ (++ ldflags)
                                 & L.extraLibs    %~ (++ libs)
                                 & L.extraLibDirs %~ (++ lib_dirs)
             _            -> lib
       let lib' = (fmap . fmap) tweakLib (condLibrary  gpd)
       let gpd' = gpd { condLibrary = lib' }
       confHook simpleUserHooks (gpd', hbi) flags
  | otherwise = do
      confHook simpleUserHooks (gpd, hbi) flags

tokenizeArguments :: String -> [String]
tokenizeArguments = words
