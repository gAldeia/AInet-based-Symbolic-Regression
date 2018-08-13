{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_SymbolicRegression (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/mnt/c/Users/Guilherme/Desktop/AInet-based-Symbolic-Regression/SymbolicRegression/.stack-work/install/x86_64-linux/lts-12.2/8.4.3/bin"
libdir     = "/mnt/c/Users/Guilherme/Desktop/AInet-based-Symbolic-Regression/SymbolicRegression/.stack-work/install/x86_64-linux/lts-12.2/8.4.3/lib/x86_64-linux-ghc-8.4.3/SymbolicRegression-0.1.1.0-A6mhWinC6IcKsFtZ7n6Hua-SymbolicRegression"
dynlibdir  = "/mnt/c/Users/Guilherme/Desktop/AInet-based-Symbolic-Regression/SymbolicRegression/.stack-work/install/x86_64-linux/lts-12.2/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/mnt/c/Users/Guilherme/Desktop/AInet-based-Symbolic-Regression/SymbolicRegression/.stack-work/install/x86_64-linux/lts-12.2/8.4.3/share/x86_64-linux-ghc-8.4.3/SymbolicRegression-0.1.1.0"
libexecdir = "/mnt/c/Users/Guilherme/Desktop/AInet-based-Symbolic-Regression/SymbolicRegression/.stack-work/install/x86_64-linux/lts-12.2/8.4.3/libexec/x86_64-linux-ghc-8.4.3/SymbolicRegression-0.1.1.0"
sysconfdir = "/mnt/c/Users/Guilherme/Desktop/AInet-based-Symbolic-Regression/SymbolicRegression/.stack-work/install/x86_64-linux/lts-12.2/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SymbolicRegression_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SymbolicRegression_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SymbolicRegression_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SymbolicRegression_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SymbolicRegression_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SymbolicRegression_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
