{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_bt_graph_dp (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/mnt/c/Users/polfo/Documents/UNI/TFG/Pruebas Haskell/experiments/IEBT/bt-graph-dpNew/.stack-work/install/x86_64-linux/ab5d921545b872b9b6ed7f83740d01a90a5d5bbde9ce0bc363dd77c2b9df628d/8.10.4/bin"
libdir     = "/mnt/c/Users/polfo/Documents/UNI/TFG/Pruebas Haskell/experiments/IEBT/bt-graph-dpNew/.stack-work/install/x86_64-linux/ab5d921545b872b9b6ed7f83740d01a90a5d5bbde9ce0bc363dd77c2b9df628d/8.10.4/lib/x86_64-linux-ghc-8.10.4/bt-graph-dp-0.1.0.0-2mKprgZbYzx5GqLSHfdmMK-benchmark"
dynlibdir  = "/mnt/c/Users/polfo/Documents/UNI/TFG/Pruebas Haskell/experiments/IEBT/bt-graph-dpNew/.stack-work/install/x86_64-linux/ab5d921545b872b9b6ed7f83740d01a90a5d5bbde9ce0bc363dd77c2b9df628d/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/mnt/c/Users/polfo/Documents/UNI/TFG/Pruebas Haskell/experiments/IEBT/bt-graph-dpNew/.stack-work/install/x86_64-linux/ab5d921545b872b9b6ed7f83740d01a90a5d5bbde9ce0bc363dd77c2b9df628d/8.10.4/share/x86_64-linux-ghc-8.10.4/bt-graph-dp-0.1.0.0"
libexecdir = "/mnt/c/Users/polfo/Documents/UNI/TFG/Pruebas Haskell/experiments/IEBT/bt-graph-dpNew/.stack-work/install/x86_64-linux/ab5d921545b872b9b6ed7f83740d01a90a5d5bbde9ce0bc363dd77c2b9df628d/8.10.4/libexec/x86_64-linux-ghc-8.10.4/bt-graph-dp-0.1.0.0"
sysconfdir = "/mnt/c/Users/polfo/Documents/UNI/TFG/Pruebas Haskell/experiments/IEBT/bt-graph-dpNew/.stack-work/install/x86_64-linux/ab5d921545b872b9b6ed7f83740d01a90a5d5bbde9ce0bc363dd77c2b9df628d/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bt_graph_dp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bt_graph_dp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bt_graph_dp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bt_graph_dp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bt_graph_dp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bt_graph_dp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
