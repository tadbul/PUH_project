module Paths_PUH_Project (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\tauri\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\tauri\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.2\\PUH-Project-0.1.0.0-BH5HKuFkKVuCNj2tku4HOF"
datadir    = "C:\\Users\\tauri\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.2\\PUH-Project-0.1.0.0"
libexecdir = "C:\\Users\\tauri\\AppData\\Roaming\\cabal\\PUH-Project-0.1.0.0-BH5HKuFkKVuCNj2tku4HOF"
sysconfdir = "C:\\Users\\tauri\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PUH_Project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PUH_Project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "PUH_Project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PUH_Project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PUH_Project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
