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

bindir     = "/home/hpacheco/.cabal/bin"
libdir     = "/home/hpacheco/.cabal/lib/x86_64-linux-ghc-7.8.3/weak-hashtables-1.2.0.2"
datadir    = "/home/hpacheco/.cabal/share/x86_64-linux-ghc-7.8.3/weak-hashtables-1.2.0.2"
libexecdir = "/home/hpacheco/.cabal/libexec"
sysconfdir = "/home/hpacheco/.cabal/etc"

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
