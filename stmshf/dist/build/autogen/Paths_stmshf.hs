module Paths_stmshf (
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
version = Version {versionBranch = [0,1,3,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/hpacheco/Library/Haskell/bin"
libdir     = "/Users/hpacheco/Library/Haskell/ghc-7.8.3-x86_64/lib/stmshf-0.1.3.1"
datadir    = "/Users/hpacheco/Library/Haskell/share/ghc-7.8.3-x86_64/stmshf-0.1.3.1"
libexecdir = "/Users/hpacheco/Library/Haskell/libexec"
sysconfdir = "/Users/hpacheco/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "stmshf_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "stmshf_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "stmshf_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "stmshf_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "stmshf_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
