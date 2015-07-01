module Paths_weak_hashtables (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,2,0,2], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/hpacheco/Library/Haskell/bin"
libdir     = "/Users/hpacheco/Library/Haskell/ghc-7.8.3-x86_64/lib/weak-hashtables-1.2.0.2"
datadir    = "/Users/hpacheco/Library/Haskell/share/ghc-7.8.3-x86_64/weak-hashtables-1.2.0.2"
libexecdir = "/Users/hpacheco/Library/Haskell/libexec"
sysconfdir = "/Users/hpacheco/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "weak_hashtables_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "weak_hashtables_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "weak_hashtables_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "weak_hashtables_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "weak_hashtables_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
