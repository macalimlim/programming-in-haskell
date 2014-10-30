module Paths_programming_in_haskell (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/macalimlim/.cabal/bin"
libdir     = "/home/macalimlim/.cabal/lib/x86_64-linux-ghc-7.8.3/programming-in-haskell-0.1.0.0"
datadir    = "/home/macalimlim/.cabal/share/x86_64-linux-ghc-7.8.3/programming-in-haskell-0.1.0.0"
libexecdir = "/home/macalimlim/.cabal/libexec"
sysconfdir = "/home/macalimlim/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "programming_in_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "programming_in_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "programming_in_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "programming_in_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "programming_in_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
